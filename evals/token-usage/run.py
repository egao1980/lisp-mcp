"""Token-usage eval: the same Common Lisp task with and without lisp-mcp.

Each trial runs Claude Code headless (`claude -p`) in a fresh empty directory:

  mcp    only the lisp-mcp server (strict MCP config), told to use it
  plain  no MCP server: file tools and the shell

Both arms load no user settings or plugins (`--setting-sources project` in an
empty project) and run with `--permission-mode dontAsk` plus an allow-list.
After the agent stops, an independent grader (`cl-repo test`, then a hidden
behaviour check) decides pass/fail; token use is compared over passing runs.

  uv run evals/token-usage/run.py --trials 3            # both arms, 3 each
  uv run evals/token-usage/run.py --arms mcp --trials 1 --model claude-sonnet-5-5

Every trial is a billed Claude Code session. Results: evals/token-usage/results/.
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import os
import pathlib
import shutil
import statistics
import subprocess
import tempfile
import time

HERE = pathlib.Path(__file__).resolve().parent
WORKSPACE = HERE.parents[2]  # cl-workspace (lisp-mcp/evals/token-usage → ../../..)
LAUNCHER = WORKSPACE / ".lisp-workspace" / "scripts" / "lisp-mcp.sh"

COMMON_TOOLS = [
    "Read", "Write", "Edit", "Glob", "Grep",
    "Bash(cl-repo:*)", "Bash(ls:*)", "Bash(cat:*)", "Bash(mkdir:*)",
]
ARMS = {
    "mcp": {
        "tools": COMMON_TOOLS + ["mcp__lisp-mcp"],
        "hint": ("Use the lisp-mcp MCP tools for Lisp work: edit-new-file and the "
                 "edit-* tools to write and change .lisp/.asd files, source-check to "
                 "read-check them, and repl-spawn / repl-load-system / repl-run-tests "
                 "for a live image. Use cl-repo for dependencies and the final check."),
    },
    "plain": {
        "tools": COMMON_TOOLS,
        "hint": "Use the file tools and the shell (cl-repo) for this work.",
    },
}


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


def run_agent(arm: str, task: str, run_dir: pathlib.Path, model: str | None,
              timeout: int) -> dict:
    cfg = run_dir.parent / f"{run_dir.name}.mcp.json"
    cfg.write_text(json.dumps(mcp_config(run_dir) if arm == "mcp" else {"mcpServers": {}}))
    cmd = ["claude", "-p", task,
           "--output-format", "stream-json", "--verbose",
           "--no-session-persistence",
           "--setting-sources", "project",
           "--strict-mcp-config", "--mcp-config", str(cfg),
           "--permission-mode", "dontAsk",
           "--allowedTools", *ARMS[arm]["tools"],
           "--append-system-prompt", ARMS[arm]["hint"]]
    if model:
        cmd += ["--model", model]
    started = time.monotonic()
    proc = subprocess.run(cmd, cwd=run_dir, capture_output=True, text=True,
                          timeout=timeout)
    wall = time.monotonic() - started
    out, tools = None, {}
    for line in proc.stdout.splitlines():
        try:
            ev = json.loads(line)
        except json.JSONDecodeError:
            continue
        if ev.get("type") == "result":
            out = ev
        elif ev.get("type") == "assistant":
            for part in (ev.get("message") or {}).get("content") or []:
                if isinstance(part, dict) and part.get("type") == "tool_use":
                    tools[part["name"]] = tools.get(part["name"], 0) + 1
    if out is None:
        out = {"error": "no result event from claude", "stderr": proc.stderr[-2000:],
               "stdout": proc.stdout[-2000:]}
    out["tool_counts"] = tools
    out["wall_s"] = round(wall, 1)
    return out


def grade(run_dir: pathlib.Path, timeout: int) -> dict:
    def sh(*args: str) -> subprocess.CompletedProcess:
        return subprocess.run(list(args), cwd=run_dir, capture_output=True,
                              text=True, timeout=timeout)
    tests = sh("cl-repo", "test")
    grader = HERE / "tasks" / "ring-buffer-grade.lisp"
    hidden = sh("cl-repo", "run", "--", "ros", "-l", str(grader))
    line = next((l for l in hidden.stdout.splitlines() if l.startswith("GRADE ")), "GRADE ?")
    return {"own_tests_pass": tests.returncode == 0,
            "hidden_pass": hidden.returncode == 0 and line == "GRADE PASS",
            "hidden_line": line}


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
    lines = ["arm    n  pass  median_all_input  median_output  median_cost  median_turns  median_wall_s"]
    for arm in ARMS:
        rs = [r for r in rows if r["arm"] == arm]
        ok = [r for r in rs if r["grade"]["hidden_pass"]]
        if not rs:
            continue
        def med(key, sub=None):
            vals = [(r[sub][key] if sub else r[key]) for r in ok]
            return statistics.median(vals) if vals else float("nan")
        lines.append(f"{arm:<6} {len(rs):<2} {len(ok):<5} {med('all_input', 'usage'):>16,.0f} "
                     f"{med('output', 'usage'):>14,.0f} {med('cost_usd'):>12.3f} "
                     f"{med('turns'):>13.0f} {med('wall_s'):>14.0f}")
    lines.append("(medians over trials whose hidden grade passed)")
    return "\n".join(lines)


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--trials", type=int, default=3)
    ap.add_argument("--arms", nargs="+", choices=list(ARMS), default=list(ARMS))
    ap.add_argument("--model", default=None, help="claude --model (default: CLI default)")
    ap.add_argument("--task", default="ring-buffer")
    ap.add_argument("--agent-timeout", type=int, default=1800)
    ap.add_argument("--grade-timeout", type=int, default=900)
    ap.add_argument("--keep", action="store_true", help="keep run directories")
    args = ap.parse_args()

    task = (HERE / "tasks" / f"{args.task}.md").read_text()
    results_dir = HERE / "results"
    results_dir.mkdir(exist_ok=True)
    stamp = dt.datetime.now().strftime("%Y%m%d-%H%M%S")
    out_path = results_dir / f"{stamp}-{args.task}.jsonl"
    root = pathlib.Path(tempfile.mkdtemp(prefix="lisp-mcp-eval-"))
    rows = []
    # Interleave arms so drift (cache warmth, load) does not favour one arm.
    for trial in range(args.trials):
        for arm in args.arms:
            run_dir = root / f"{arm}-{trial}" / "ring-buffer"
            run_dir.mkdir(parents=True)
            print(f"[{arm} #{trial}] running in {run_dir}", flush=True)
            out = run_agent(arm, task, run_dir, args.model, args.agent_timeout)
            g = grade(run_dir, args.grade_timeout)
            row = {"arm": arm, "trial": trial, "task": args.task, "model": args.model,
                   "usage": usage_of(out), "cost_usd": out.get("total_cost_usd"),
                   "turns": out.get("num_turns"), "wall_s": out.get("wall_s"),
                   "terminal_reason": out.get("terminal_reason"),
                   "permission_denials": len(out.get("permission_denials") or []),
                   "tools": out.get("tool_counts", {}),
                   "error": out.get("error"), "grade": g, "dir": str(run_dir)}
            rows.append(row)
            with out_path.open("a") as f:
                f.write(json.dumps(row) + "\n")
            print(f"[{arm} #{trial}] pass={g['hidden_pass']} tests={g['own_tests_pass']} "
                  f"in={row['usage']['all_input']:,} out={row['usage']['output']:,} "
                  f"cost={row['cost_usd']} turns={row['turns']} ({g['hidden_line']})", flush=True)
            print(f"[{arm} #{trial}] tools: {row['tools']}", flush=True)
    print()
    print(summarize(rows))
    print(f"\nresults: {out_path}")
    if not args.keep:
        shutil.rmtree(root, ignore_errors=True)
    else:
        print(f"run dirs: {root}")


if __name__ == "__main__":
    main()
