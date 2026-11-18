"""Token-usage eval: the same Common Lisp tasks with and without lisp-mcp.

Each trial runs Claude Code headless (`claude -p`) in a fresh directory:

  mcp    only the lisp-mcp server (strict MCP config), told to use it
  plain  no MCP server: file tools and the shell

Both arms load no user settings or plugins (`--setting-sources project` in a
project without settings) and run with `--permission-mode dontAsk` plus an
allow-list. A task (tasks/<name>.json + .md) may start from a seed directory
(seeds/<name>), from a third-party library's source taken out of the cl-repo
install tree, and from planted patches. After the agent stops, an independent
grade decides pass/fail: the project's own `cl-repo test`, a hidden behaviour
check (tasks/<name>-grade.lisp), and files that must stay unchanged.

Besides tokens, cost, turns and wall time, each trial records how the agent
worked: tool calls by name, reader errors seen in tool output (unbalanced
parentheses, end of file), lisp-mcp refusals of unbalanced input, whole-file
rewrites of existing .lisp/.asd files, characters written, and characters of
tool output read back.

  uv run evals/token-usage/run.py --trials 3                       # every task, both arms
  uv run evals/token-usage/run.py --tasks ledger-edit --trials 5
  uv run evals/token-usage/run.py --arms mcp --tasks ledger-debug --trials 1 --keep

Every trial is a billed Claude Code session. Results: evals/token-usage/results/.
"""

from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import json
import os
import pathlib
import re
import shutil
import signal
import statistics
import subprocess
import tempfile
import time

HERE = pathlib.Path(__file__).resolve().parent
WORKSPACE = HERE.parents[2]  # cl-workspace (lisp-mcp/evals/token-usage → ../../..)
LAUNCHER = WORKSPACE / ".lisp-workspace" / "scripts" / "lisp-mcp.sh"
INSTALL_TREE = pathlib.Path(os.environ.get("CL_REPO_SYSTEMS",
                                           pathlib.Path.home() / ".local/share/cl-repository/systems"))
TASKS = ["ring-buffer", "ledger-edit", "ledger-debug", "local-time-quarter"]

COMMON_TOOLS = [
    "Read", "Write", "Edit", "Glob", "Grep",
    "Bash(cl-repo:*)", "Bash(ls:*)", "Bash(cat:*)", "Bash(mkdir:*)",
    "Bash(grep:*)", "Bash(head:*)", "Bash(tail:*)", "Bash(wc:*)",
]
ARMS = {
    "mcp": {
        "tools": COMMON_TOOLS + ["mcp__lisp-mcp"],
        "hint": ("Use the lisp-mcp MCP tools for Lisp work: source-outline / source-read to "
                 "read forms, the edit-* tools (edit-replace, edit-insert, edit-rename-symbol, "
                 "edit-new-file, ...) to change .lisp/.asd files instead of rewriting them, "
                 "source-check to read-check them, and repl-spawn / repl-load-system / "
                 "repl-run-tests with the debug-* and inspect tools to run and debug in a live "
                 "image. Use cl-repo for dependencies and the final check."),
    },
    "plain": {
        "tools": COMMON_TOOLS,
        "hint": "Use the file tools and the shell (cl-repo) for this work.",
    },
}

READER_ERROR = re.compile(r"READ error|end of file on|unmatched close parenthesis|"
                          r"Unbalanced|reader-error|READER-ERROR", re.I)
MCP_REFUSAL = re.compile(r"does not read as balanced|would unbalance|edit-input-unreadable", re.I)
LISP_FILE = re.compile(r"\.(lisp|asd)$")


def run_group(cmd: list[str], cwd: pathlib.Path, timeout: int) -> tuple[int, str, str, bool]:
    """Run CMD in its own session; on timeout, and always at the end, kill the
    whole process group: cl-repo -> ros -> sbcl, or claude -> lisp-mcp -> its
    spawned images, would otherwise outlive the trial.
    -> (returncode, stdout, stderr, timed_out)."""
    proc = subprocess.Popen(cmd, cwd=cwd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                            text=True, start_new_session=True)
    try:
        out, err = proc.communicate(timeout=timeout)
        timed_out = False
    except subprocess.TimeoutExpired:
        os.killpg(proc.pid, signal.SIGKILL)
        out, err = proc.communicate()
        timed_out = True
    try:
        os.killpg(proc.pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    return proc.returncode, out or "", err or "", timed_out


# ---------------------------------------------------------------- task setup

def load_task(name: str) -> dict:
    cfg = json.loads((HERE / "tasks" / f"{name}.json").read_text())
    cfg["name"] = name
    cfg["prompt"] = (HERE / "tasks" / f"{name}.md").read_text()
    return cfg


def cl_repo_source(system: str, version: str) -> pathlib.Path:
    path = INSTALL_TREE / system / version
    if not path.exists():
        subprocess.run(["cl-repo", "install", f"{system}:{version}"], check=True,
                       capture_output=True, text=True)
    return path


def prepare(task: dict, run_dir: pathlib.Path) -> dict:
    """Fill RUN_DIR for TASK. → {relative path: sha256} of files that must not change."""
    if "seed" in task:
        shutil.copytree(HERE / task["seed"], run_dir, dirs_exist_ok=True)
    if "cl-repo-source" in task:
        src = task["cl-repo-source"]
        shutil.copytree(cl_repo_source(src["system"], src["version"]), run_dir,
                        dirs_exist_ok=True)
    for rel in task.get("drop", []):
        p = run_dir / rel
        if p.is_dir():
            shutil.rmtree(p)
        elif p.exists():
            p.unlink()
    for rel, old, new in task.get("patch", []):
        p = run_dir / rel
        text = p.read_text()
        if text.count(old) != 1:
            raise SystemExit(f"task {task['name']}: patch for {rel} matches {text.count(old)} times")
        p.write_text(text.replace(old, new))
    protected = task.get("unchanged", [])
    return {rel: hashlib.sha256((run_dir / rel).read_bytes()).hexdigest() for rel in protected}


def existing_lisp_files(run_dir: pathlib.Path) -> set[str]:
    return {str(p.resolve()) for p in run_dir.rglob("*") if LISP_FILE.search(p.name)}


# ---------------------------------------------------------------- the agent

def mcp_config(run_dir: pathlib.Path) -> dict:
    """The workspace launcher when present, else LISP_MCP_COMMAND or `lisp-mcp`."""
    if os.environ.get("LISP_MCP_COMMAND"):
        command, args = os.environ["LISP_MCP_COMMAND"], []
    elif LAUNCHER.exists():
        command, args = "bash", [str(LAUNCHER)]
    else:
        command, args = "lisp-mcp", []
    return {"mcpServers": {"lisp-mcp": {"command": command,
                                        "args": args + ["--root", str(run_dir)]}}}


def _text_of(content) -> str:
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        return "\n".join(c.get("text", "") for c in content if isinstance(c, dict))
    return ""


def _written_chars(name: str, inp: dict) -> int:
    if name == "Write":
        return len(inp.get("content", ""))
    if name == "Edit":
        return len(inp.get("new_string", ""))
    if name == "MultiEdit":
        return sum(len(e.get("new_string", "")) for e in inp.get("edits", []))
    if name.startswith("mcp__lisp-mcp__edit-"):
        return len(inp.get("code", "") or "") + len(inp.get("new_name", "") or "")
    return 0


def run_agent(arm: str, task: dict, run_dir: pathlib.Path, model: str | None,
              timeout: int) -> dict:
    cfg = run_dir.parent / f"{run_dir.name}.mcp.json"
    cfg.write_text(json.dumps(mcp_config(run_dir) if arm == "mcp" else {"mcpServers": {}}))
    cmd = ["claude", "-p", task["prompt"],
           "--output-format", "stream-json", "--verbose",
           "--no-session-persistence",
           "--setting-sources", "project",
           "--strict-mcp-config", "--mcp-config", str(cfg),
           "--permission-mode", "dontAsk",
           "--allowedTools", *ARMS[arm]["tools"],
           "--append-system-prompt", ARMS[arm]["hint"]]
    if model:
        cmd += ["--model", model]
    seen_files = existing_lisp_files(run_dir)
    started = time.monotonic()
    _, stdout, stderr, timed_out = run_group(cmd, run_dir, timeout)
    wall = time.monotonic() - started

    result, tools = None, {}
    work = {"reader_errors": 0, "mcp_refusals": 0, "full_rewrites": 0,
            "chars_written": 0, "tool_output_chars": 0}
    for line in stdout.splitlines():
        try:
            ev = json.loads(line)
        except json.JSONDecodeError:
            continue
        kind = ev.get("type")
        if kind == "result":
            result = ev
        elif kind == "assistant":
            for part in (ev.get("message") or {}).get("content") or []:
                if not (isinstance(part, dict) and part.get("type") == "tool_use"):
                    continue
                name, inp = part["name"], part.get("input") or {}
                tools[name] = tools.get(name, 0) + 1
                work["chars_written"] += _written_chars(name, inp)
                path = inp.get("file_path")
                if name == "Write" and path and LISP_FILE.search(path):
                    full = str(pathlib.Path(path).resolve())
                    if full in seen_files:
                        work["full_rewrites"] += 1
                    seen_files.add(full)
        elif kind == "user":
            for part in (ev.get("message") or {}).get("content") or []:
                if isinstance(part, dict) and part.get("type") == "tool_result":
                    text = _text_of(part.get("content"))
                    work["tool_output_chars"] += len(text)
                    if READER_ERROR.search(text):
                        work["reader_errors"] += 1
                    if MCP_REFUSAL.search(text):
                        work["mcp_refusals"] += 1
    out = result or {"error": "no result event from claude", "stderr": stderr[-2000:]}
    if timed_out:
        out["error"] = f"agent timed out after {timeout}s"
    out.update(tool_counts=tools, work=work, wall_s=round(wall, 1))
    return out


# ---------------------------------------------------------------- grading

def grade(task: dict, run_dir: pathlib.Path, protected: dict, timeout: int) -> dict:
    def sh(*args: str) -> subprocess.CompletedProcess:
        code, out, err, timed_out = run_group(list(args), run_dir, timeout)
        return subprocess.CompletedProcess(args, 124 if timed_out else code, out, err)
    tests = sh("cl-repo", "test")
    grader = HERE / "tasks" / task["grader"]
    hidden = sh("cl-repo", "run", "--", "ros", "-l", str(grader))
    line = next((l for l in hidden.stdout.splitlines() if l.startswith("GRADE ")),
                "GRADE ? (no grade line)")
    changed = [rel for rel, digest in protected.items()
               if not (run_dir / rel).exists()
               or hashlib.sha256((run_dir / rel).read_bytes()).hexdigest() != digest]
    ok = tests.returncode == 0 and hidden.returncode == 0 and line == "GRADE PASS" and not changed
    return {"pass": ok, "own_tests_pass": tests.returncode == 0,
            "hidden_line": line, "changed_protected": changed}


# ---------------------------------------------------------------- report

def usage_of(out: dict) -> dict:
    totals = {"input": 0, "cache_write": 0, "cache_read": 0, "output": 0}
    for m in (out.get("modelUsage") or {}).values():
        totals["input"] += m.get("inputTokens", 0)
        totals["cache_write"] += m.get("cacheCreationInputTokens", 0)
        totals["cache_read"] += m.get("cacheReadInputTokens", 0)
        totals["output"] += m.get("outputTokens", 0)
    totals["all_input"] = totals["input"] + totals["cache_write"] + totals["cache_read"]
    return totals


def summarize(rows: list[dict]) -> str:
    head = (f"{'task':<20} {'arm':<6} {'n':>2} {'pass':>4} {'all_input':>10} {'output':>7} "
            f"{'cost$':>6} {'turns':>5} {'wall_s':>6} {'rd_err':>6} {'refuse':>6} "
            f"{'rewrite':>7} {'written':>8} {'read_back':>9}")
    lines = [head]
    for task in dict.fromkeys(r["task"] for r in rows):
        for arm in ARMS:
            rs = [r for r in rows if r["task"] == task and r["arm"] == arm]
            if not rs:
                continue
            ok = [r for r in rs if r["grade"]["pass"]]

            def med(f):
                vals = [f(r) for r in ok]
                return statistics.median(vals) if vals else float("nan")
            lines.append(
                f"{task:<20} {arm:<6} {len(rs):>2} {len(ok):>4} "
                f"{med(lambda r: r['usage']['all_input']):>10,.0f} "
                f"{med(lambda r: r['usage']['output']):>7,.0f} "
                f"{med(lambda r: r['cost_usd'] or 0):>6.2f} {med(lambda r: r['turns'] or 0):>5.0f} "
                f"{med(lambda r: r['wall_s']):>6.0f} "
                f"{med(lambda r: r['work']['reader_errors']):>6.0f} "
                f"{med(lambda r: r['work']['mcp_refusals']):>6.0f} "
                f"{med(lambda r: r['work']['full_rewrites']):>7.0f} "
                f"{med(lambda r: r['work']['chars_written']):>8,.0f} "
                f"{med(lambda r: r['work']['tool_output_chars']):>9,.0f}")
    lines.append("(medians over passing trials; rd_err = reader errors seen, refuse = lisp-mcp "
                 "refusals of unbalanced input, rewrite = Write over an existing .lisp/.asd)")
    return "\n".join(lines)


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--trials", type=int, default=3)
    ap.add_argument("--arms", nargs="+", choices=list(ARMS), default=list(ARMS))
    ap.add_argument("--tasks", nargs="+", choices=TASKS, default=TASKS)
    ap.add_argument("--model", default=None, help="claude --model (default: CLI default)")
    ap.add_argument("--agent-timeout", type=int, default=2400)
    ap.add_argument("--grade-timeout", type=int, default=900)
    ap.add_argument("--keep", action="store_true", help="keep run directories")
    ap.add_argument("--prepare-only", action="store_true",
                    help="set up one run directory per task, print it, run no agent")
    args = ap.parse_args()

    results_dir = HERE / "results"
    results_dir.mkdir(exist_ok=True)
    stamp = dt.datetime.now().strftime("%Y%m%d-%H%M%S")
    out_path = results_dir / f"{stamp}.jsonl"
    root = pathlib.Path(tempfile.mkdtemp(prefix="lisp-mcp-eval-"))
    tasks = [load_task(t) for t in args.tasks]

    if args.prepare_only:
        for task in tasks:
            run_dir = root / task["name"] / task["dir"]
            run_dir.mkdir(parents=True)
            prepare(task, run_dir)
            print(f"{task['name']}: {run_dir}")
        return

    rows = []
    # Interleave arms (and tasks) so drift does not favour one arm.
    for trial in range(args.trials):
        for task in tasks:
            for arm in args.arms:
                run_dir = root / f"{task['name']}-{arm}-{trial}" / task["dir"]
                run_dir.mkdir(parents=True)
                protected = prepare(task, run_dir)
                print(f"[{task['name']} {arm} #{trial}] {run_dir}", flush=True)
                out = run_agent(arm, task, run_dir, args.model, args.agent_timeout)
                g = grade(task, run_dir, protected, args.grade_timeout)
                row = {"task": task["name"], "arm": arm, "trial": trial, "model": args.model,
                       "usage": usage_of(out), "cost_usd": out.get("total_cost_usd"),
                       "turns": out.get("num_turns"), "wall_s": out.get("wall_s"),
                       "work": out.get("work"), "tools": out.get("tool_counts"),
                       "permission_denials": len(out.get("permission_denials") or []),
                       "terminal_reason": out.get("terminal_reason"),
                       "error": out.get("error"), "grade": g, "dir": str(run_dir)}
                rows.append(row)
                with out_path.open("a") as f:
                    f.write(json.dumps(row) + "\n")
                w = row["work"] or {}
                print(f"  pass={g['pass']} ({g['hidden_line']}, own tests {g['own_tests_pass']}"
                      f"{', changed ' + ','.join(g['changed_protected']) if g['changed_protected'] else ''})"
                      f" in={row['usage']['all_input']:,} out={row['usage']['output']:,}"
                      f" cost={row['cost_usd']} turns={row['turns']}"
                      f" rd_err={w.get('reader_errors')} rewrites={w.get('full_rewrites')}", flush=True)
                print(f"  tools: {row['tools']}", flush=True)
    print()
    print(summarize(rows))
    print(f"\nresults: {out_path}")
    if args.keep:
        print(f"run dirs: {root}")
    else:
        shutil.rmtree(root, ignore_errors=True)


if __name__ == "__main__":
    main()
