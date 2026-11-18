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

| Task | Starts from | What it exercises |
|------|-------------|-------------------|
| `ring-buffer` | empty directory | writing new files (baseline) |
| `ledger-edit` | `seeds/ledger`, about 300 lines | editing in place: a rename across files, a new condition with a `convert` restart, a new function, tests |
| `ledger-debug` | the same seed with two planted bugs (the last month is dropped; an empty month divides by zero several calls deep) | debugging; `tests/ledger-test.lisp` must stay unchanged |
| `local-time-quarter` | local-time 1.0.6, copied from the cl-repo install tree at run time | a feature in a big third-party library: `timestamp-quarter`, plus a `:quarter` case in the `ecase` at the bottom of a deeply nested function in the 2,084-line `src/local-time.lisp`; fiasco tests |

A task is `tasks/<name>.json`, a prompt `tasks/<name>.md`, and a grader `tasks/<name>-grade.lisp`. The JSON fields:

| Field | Meaning |
|-------|---------|
| `dir` | name of the run directory |
| `seed` | directory under `evals/token-usage/` copied into the run directory |
| `cl-repo-source` | `{system, version}`, a library's source copied out of the cl-repo install tree |
| `drop` | files or directories removed after copying |
| `patch` | `[file, old, new]` replacements; each `old` must match exactly once |
| `unchanged` | files the agent must not modify |
| `grader` | the grader file |

A trial passes only when all three hold:
- the project's own `cl-repo test` passes;
- the grader prints `GRADE PASS`;
- no file listed in `unchanged` was modified.

Every grader was checked twice: it fails on the task's starting state and passes on a reference solution. Agents and graders run in their own process group, which the harness kills at the end of each trial or on timeout, so no `sbcl` survives a trial.

## Metrics

Per trial:

- **Tokens.** Fresh input, cache writes, cache reads and output, from the result event's `modelUsage`. Also cost, turns and wall time.
- **Tool calls** by name.
- **`rd_err`.** Tool results that show a reader error: unbalanced parentheses, or an end-of-file read error.
- **`refuse`.** lisp-mcp refusing an edit whose input does not read as balanced. Nothing was written in these cases.
- **`rewrite`.** `Write` over a `.lisp` or `.asd` file that already existed.
- **`written`.** Characters the agent sent through `Write`, `Edit` and the `edit-*` tools.
- **`read_back`.** Characters of tool output the agent read: file contents, command output, MCP results.

## Validation runs (2026-11-18, Opus 5.5, one trial per arm)

| task | arm | pass | input tokens (fresh + cache) | cache read per turn | output | cost | turns | read back | written | rewrites |
|------|-----|------|------------------------------|---------------------|--------|------|-------|-----------|---------|----------|
| ring-buffer | mcp | ✓ | 127k | — | 3.4k | $0.22 | 8–9 | — | — | — |
| ring-buffer | plain | ✓ | 119k | — | 5.5k | $0.25 | 7 | — | — | — |
| ledger-edit | mcp | ✓ | 345k | 23.3k | 4.1k | $0.30 | 14 | 13k | 3.7k | 0 |
| ledger-edit | plain | ✓ | 231k | 11.4k | 6.2k | $0.38 | 18 | 31k | 6.5k | 1 |
| ledger-debug | mcp | ✓ | 211k | 23.6k | 1.7k | $0.25 | 8 | 22k | 0.5k | 0 |
| ledger-debug | plain | ✓ | 130k | 9.1k | 2.0k | $0.22 | 12 | 23k | 0.2k | 0 |
| local-time-quarter | mcp | ✓ | 458k | 20.6k | 5.9k | $0.41 | 21 | 25k | 3.6k | 0 (1 refusal) |
| local-time-quarter | plain | ✓ | 281k | 14.0k | 4.7k | $0.38 | 18 | 41k | 3.0k | 0 |

These are single trials, so they are signals, not results:

- **lisp-mcp cuts what the agent reads and writes.**
  - ledger-edit: tool output read back fell from 31k to 13k characters; output tokens from 6.2k to 4.1k; characters written from 6.5k to 3.7k; whole-file rewrites from 1 to 0.
  - local-time: read back fell from 41k to 25k characters. The agent used `source-read`, `edit-insert` and `edit-add-export` instead of reading the 2,084-line file.
  - A rename was one `edit-rename-symbol` call.
- **The tool catalog costs about 10k tokens of cached prefix on every turn.** The cache read per turn is 21–24k with lisp-mcp against 9–14k without. That is lisp-mcp's 63 tool definitions. Cache reads are billed at a tenth of the input price, so cost comes out even or lower (ledger-edit $0.30 vs $0.38), but raw input is 1.5–1.7× higher. A smaller catalog is the clearest win left: a core tool set, or shorter argument schemas.
- **The unbalanced-edit guard fired once.** In local-time, lisp-mcp refused one edit whose code did not read as balanced, and nothing was written. The plain arm hit no reader errors on these tasks, so more trials are needed to compare.
- **ledger-debug did not need the debugger.** Both arms fixed both bugs from `cl-repo test` output: the division by zero's backtrace is printed there. A task to compare debugging needs a failure whose cause the test output does not show. For example, wrong state built up across calls, an error inside a callback or thread, or a condition handled and turned into a wrong value, where `debug-frame-locals` or `inspect` pays off.

Run at least 5 trials per arm before drawing conclusions:

```bash
uv run evals/token-usage/run.py --trials 5    # about 40 sessions, roughly $12-15 on Opus 5.5
```
