# Token-usage eval: with and without lisp-mcp

This eval runs the same Common Lisp task through Claude Code in headless mode (`claude -p`) in two arms:

| Arm | Tools |
|-----|-------|
| `mcp` | lisp-mcp, the only MCP server (`--strict-mcp-config`), plus file tools and `cl-repo`. The agent is told to use lisp-mcp for Lisp work. |
| `plain` | file tools and the shell (`cl-repo`, `ls`, `cat`, `mkdir`), no MCP server |

Both arms run the same way:
- no user settings or plugins (`--setting-sources project`, in an empty project directory);
- `--permission-mode dontAsk` with an allow-list;
- the same task prompt;
- arms alternate trial by trial, so cache warmth and machine load don't favour one.

An independent grader decides whether a trial passed. It runs `cl-repo test` in the agent's directory, then a hidden behaviour check (`tasks/<task>-grade.lisp`). Token figures are compared only across trials that passed.

```bash
uv run evals/token-usage/run.py --trials 5                       # both arms
uv run evals/token-usage/run.py --arms mcp --trials 1 --keep     # one run, keep its directory
uv run evals/token-usage/run.py --trials 5 --model claude-sonnet-5-5
```

Each trial is a billed Claude Code session. A ring-buffer trial costs about $0.25 on Opus 5.5.

The lisp-mcp server comes from the workspace launcher, `.lisp-workspace/scripts/lisp-mcp.sh`. Without the launcher, it comes from `$LISP_MCP_COMMAND`, else from `lisp-mcp` on `PATH`.

Each trial writes a JSON line to `results/` with:
- token usage: fresh input, cache writes, cache reads, output;
- cost;
- turns;
- wall time;
- tool calls by name;
- permission denials;
- the grade.

The summary prints medians per arm over passing trials.

## Tasks

| Task | What it exercises |
|------|-------------------|
| `ring-buffer` | A new system from an empty directory: package, ASDF, a condition with a restart, Rove tests. |

**Caveat.** `ring-buffer` is mostly writing new files. The agent writes a few files and the tests pass, so lisp-mcp's strengths are barely used:
- structural edits of existing code;
- stored output that can be read in slices instead of re-running a build;
- the debugger as a value;
- jobs for long test runs.

The validation runs (2026-11-18, Opus 5.5, one trial each) show it:

| arm | pass | input tokens (fresh + cache) | output tokens | cost | turns | tools |
|-----|------|------------------------------|---------------|------|-------|-------|
| mcp | ✓ | 126,901 / 127,348 | 3,410 / 3,505 | $0.22 / $0.23 | 8 / 9 | 4 × `edit-new-file`, 3 × `Bash` (`cl-repo`), 1 × `ToolSearch` |
| plain | ✓ | 118,944 | 5,499 | $0.25 | 7 | — |

With one trial per arm there is no conclusion. To compare where lisp-mcp should matter, add tasks that start from a seeded project:
- fix a failing test in a larger file;
- rename across files;
- diagnose an error from a long build log.

Each task is a `tasks/<name>.md` prompt plus a `tasks/<name>-grade.lisp` that prints `GRADE PASS` or `GRADE FAIL …`.
