# lisp-mcp

A Common Lisp [MCP](https://modelcontextprotocol.io) server with three layers. Built on [`mcp-protocol`](https://github.com/egao1980/mcp-protocol).

| Layer | Library | What it gives an agent |
|-------|---------|------------------------|
| **Source** (read only) | [`lisp-source-protocol`](https://github.com/egao1980/lisp-source-protocol) | Outline, form text and file hash, read-check and lint, paren imbalances with a suggested fix, static definitions, references and packages. Never evaluates (`#.` refused) and never interns. |
| **Edit** (structural) | same | Replace, insert, delete, wrap, splice, raise, slurp and barf, transpose, move (also across files), rename-symbol, add-export, repair, new-file. Inserted code is re-indented. Each write is re-read first and refused if it would unbalance the file or if `expected_hash` is stale. |
| **Live image** | [`repl-protocol`](https://github.com/egao1980/repl-protocol) + [Swank](https://github.com/egao1980/repl-backend-swank) / [Slynk](https://github.com/egao1980/repl-backend-slynk) | Eval, compile (notes mapped to file positions), load-system, run-tests, debugger (a value, not a hang) with stepping, inspector, xref, threads, Slynk channels. The image is a separate process, so this server never runs project code. |

## Run

```bash
ros install egao1980/lisp-mcp            # or put the checkout on the ASDF path
lisp-mcp --root ~/src/my-project         # stdio
lisp-mcp --transport http --port 3000    # Streamable HTTP (loads lisp-mcp/http)
lisp-mcp --spawn swank                   # start with a fresh SBCL+Swank image attached
lisp-mcp --attach 127.0.0.1:4005         # or attach to your running SLIME / Sly (--backend slynk)
```

You can set the root with `--root` or `MCP_PROJECT_ROOT`. Every path is resolved inside it.

Claude Code:

```bash
claude mcp add lisp-mcp -- bash -c 'cd /path/to/lisp-mcp && exec cl-repo run -- ros +Q -- roswell/lisp-mcp.ros --root "$0"' "$PWD"
```

`cl-repo run` installs the dependencies from GHCR first and puts them on the
ASDF registry. `+Q` keeps Quicklisp out: Roswell's `-Q` turns it on.

`.mcp.json`:

```json
{ "mcpServers": { "lisp-mcp": { "command": "lisp-mcp", "args": ["--root", "."] } } }
```

Step-by-step sessions (edit → compile → debug → inspect): [cl-stack cookbook](https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/lisp-mcp.md).

## Tools

- **source:** `source-outline`, `source-read`, `source-check`, `source-find-definitions`, `source-find-references`, `source-packages`
- **edit:** `edit-replace`, `edit-insert`, `edit-delete`, `edit-wrap`, `edit-splice`, `edit-raise`, `edit-slurp`, `edit-barf`, `edit-transpose`, `edit-move`, `edit-rename-symbol`, `edit-add-export`, `edit-repair`, `edit-new-file`
- **repl:** `repl-spawn`, `repl-connect`, `repl-list`, `repl-disconnect`, `repl-eval`, `repl-compile`, `repl-load-system`, `repl-run-tests`, `repl-interrupt`, `repl-output`, `repl-describe`, `repl-documentation`, `repl-arglist`, `repl-apropos`, `repl-complete`, `repl-macroexpand`, `repl-xref`, `repl-find-definitions`, `repl-threads`, `repl-channel-open`, `repl-channel-list`, `repl-channel-close`
- **debugger:** `debug-state`, `debug-backtrace`, `debug-frame-locals`, `debug-frame-source`, `debug-eval-in-frame`, `debug-step`, `debug-invoke-restart`, `debug-return-from-frame`, `debug-restart-frame`
- **inspector:** `inspect`, `inspect-part`, `inspect-action`, `inspect-back`, `inspect-condition`, `inspect-frame-var`, `inspect-history`
- **output:** `output-read`
- **jobs:** `job-list`, `job-status`, `job-wait`, `job-cancel`

Locators: `defun foo`, `defmethod print-object (my-class t)`, `path:0.2.1`, `line:12[:col]`, `offset:N`, `hash:H`. Read-only tools carry `readOnlyHint`; tools that write files or run code carry `destructiveHint`.

**Hybrid answers.** `source-outline`, `source-find-definitions` and `source-find-references` take `live: true`: the static answer is kept, and the connected image adds what only it knows (which definitions are loaded, macro-generated definitions, who-calls / who-references through macros), marked `[image]`.

**Indentation.** Code that edits insert is re-indented in Emacs/SLIME style. The table is the standard one, plus the indentation hints the connected image sends, plus the `&body` position of every `defmacro` in the project.

**Long output.** A result over 120 lines or 10,000 characters (four times that with `verbose`) is cut, for every tool, in one place. The client gets the head and the tail (errors and summaries come last) and a note naming a stored id such as `o3`. `output-read` with that id reads the rest: `offset`/`limit` page through it, and `grep` (`|` separates alternatives, `context` lines around each match) returns only the matching lines. The same text is at `lisp-output://{id}`, `…/lines/{start}-{end}` and `…/grep/{+pattern}`, so a long build log or test run costs tokens only for the part asked for. The last 64 outputs are kept. Lines over 2,000 characters are cut in returned text, around the match for `grep`. Limits: `--max-output-lines`, `--max-output-chars`, or `LISP_MCP_MAX_OUTPUT_LINES`, `LISP_MCP_MAX_OUTPUT_CHARS` and `LISP_MCP_OUTPUT_STORE_SIZE`.

**Edits that miss say what to do next.** A locator that matches nothing lists the close definitions with their paths, or how many forms or lines the file has. A stale `expected_hash` shows the form as it now reads and the hash to retry with. It also says when the new code is already in the file, so the edit may already be applied. `compile: true` on an edit compiles the changed top-level forms in the live image and appends the compiler notes.

**Terse by default.** Output is short unless a call passes `verbose: true` (or the server runs with `--verbose` / `LISP_MCP_VERBOSE=1`). Terse mode drops the next-step hints, shows 4 backtrace frames instead of 12 in a debugger report, reports a written edit as `(+a -b lines)` instead of its diff (dry runs always show the diff), and drops Rove's passing assertions from `repl-run-tests`/`repl-load-system`, so failures, test names and the summary are what come back. Only the tools with a verbose form (`edit-*`, `repl-eval`, `repl-compile`, `repl-load-system`, `repl-run-tests`, `debug-*`, `inspect*`) carry the argument.

**Long runs become jobs.** `repl-eval`, `repl-compile`, `repl-load-system`, `repl-run-tests`, `debug-invoke-restart` and `debug-step` run in a worker thread. The call waits up to its `timeout` (60 s; 300 for `repl-load-system`, 600 for `repl-run-tests`). If the work takes longer, the evaluation keeps running in the image and the call returns at once: "still running as job jN; do not start another copy …". When the job ends the server sends a log message (logger `lisp-mcp/jobs`) and keeps the result, cut to the output limits like any result (a long one gets an `oN` id). `job-status` reads it, `job-wait` blocks for at most 300 s, `job-list` lists jobs, `job-cancel` interrupts the evaluation (`repl-interrupt`), and the job then reports the debugger state. A call with the same tool and arguments (`timeout` and `verbose` aside) as a running job does not start a second copy: it returns that job's id. An error in the worker is transferred to the call still waiting, or recorded as the job's failure. Over HTTP the completion message is sent only if the transport can notify outside a request; `job-status` always works.

**Lenient arguments.** `"false"` and `"20"` for booleans and integers, and `expectedHash` or `expected-hash` for `expected_hash`, are accepted (mcp-protocol `repair-tool-arguments`).

**Channels (Slynk).** `repl-channel-open` starts another REPL in the image with its own thread, package and value history. `repl-eval` with `channel` prints each value with its `#v<entry>:<index>` reference, which later code can use; `inspect-history` inspects one.

## Resources and notifications

| URI | Content |
|-----|---------|
| `lisp-source:///{+path}` | outline of a project file (path completion supported) |
| `repl://{connection}/output` | output the image sent outside eval results; subscribe for updates |
| `repl://{connection}/debugger` | innermost debugger level, or "Not in the debugger"; subscribe for updates |
| `repl://connections` | live connections |
| `lisp-output://{id}` (`/lines/{start}-{end}`, `/grep/{+pattern}`) | stored text of a result that was cut short |

Image output and debugger entries are also sent as log messages (`notifications/message`, logger `repl/<connection>`). Project scans, `repl-load-system`, `repl-run-tests` and whole-file `repl-compile` report progress when the request has a `progressToken`. On stdio, notifications can arrive at any time. Over HTTP they travel in the SSE response of the request that caused them; output outside a request is not delivered there.

Output from other threads of a spawned image goes to the image's own log, not to the client. It reaches `repl://…/output` for an attached SLIME/Sly session whose REPL redirects global output.

## Test

```bash
cl-repo test   # spawns SBCL+Swank and SBCL+Slynk images
```

The suite covers every layer in process, both transports end to end (stdio stream, HTTP app with SSE), resources, progress and log notifications, and live sessions against both backends.

## License

MIT
