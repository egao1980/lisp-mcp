# lisp-mcp

A Common Lisp [MCP](https://modelcontextprotocol.io) server with three layers. Built on [`mcp-protocol`](https://github.com/egao1980/mcp-protocol).

| Layer | Library | What it gives an agent |
|-------|---------|------------------------|
| **Source** (read only) | [`lisp-source-protocol`](https://github.com/egao1980/lisp-source-protocol) | Outline, form text and file hash, read-check and lint, paren imbalances with a suggested fix, static definitions and references. Never evaluates (`#.` refused) and never interns. |
| **Edit** (structural) | same | Replace, insert, delete, wrap, splice, raise, slurp and barf, transpose, rename-symbol, add-export, repair, new-file. Each write is re-read first and refused if it would unbalance the file or if `expected_hash` is stale. |
| **Live image** | [`repl-protocol`](https://github.com/egao1980/repl-protocol) + [Swank](https://github.com/egao1980/repl-backend-swank) / [Slynk](https://github.com/egao1980/repl-backend-slynk) | Eval, compile (notes mapped to file positions), load-system, run-tests, debugger (a value, not a hang), inspector, xref, threads. The image is a separate process, so this server never runs project code. |

## Run

```bash
ros install egao1980/lisp-mcp            # or put the checkout on the ASDF path
lisp-mcp --root ~/src/my-project         # stdio
lisp-mcp --transport http --port 3000    # Streamable HTTP (loads lisp-mcp/http)
lisp-mcp --spawn swank                   # start with a fresh SBCL+Swank image attached
lisp-mcp --attach 127.0.0.1:4005         # or attach to your running SLIME / Sly
```

You can set the root with `--root` or `MCP_PROJECT_ROOT`. Every path is resolved inside it.

Claude Code:

```bash
claude mcp add lisp-mcp -- ros -Q -- /path/to/lisp-mcp/roswell/lisp-mcp.ros --root "$PWD"
```

`.mcp.json`:

```json
{ "mcpServers": { "lisp-mcp": { "command": "lisp-mcp", "args": ["--root", "."] } } }
```

## Tools

- **source:** `source-outline`, `source-read`, `source-check`, `source-find-definitions`, `source-find-references`
- **edit:** `edit-replace`, `edit-insert`, `edit-delete`, `edit-wrap`, `edit-splice`, `edit-raise`, `edit-slurp`, `edit-barf`, `edit-transpose`, `edit-rename-symbol`, `edit-add-export`, `edit-repair`, `edit-new-file`
- **repl:** `repl-spawn`, `repl-connect`, `repl-list`, `repl-disconnect`, `repl-eval`, `repl-compile`, `repl-load-system`, `repl-run-tests`, `repl-interrupt`, `repl-output`, `repl-describe`, `repl-documentation`, `repl-arglist`, `repl-apropos`, `repl-complete`, `repl-macroexpand`, `repl-xref`, `repl-find-definitions`, `repl-threads`
- **debugger:** `debug-state`, `debug-backtrace`, `debug-frame-locals`, `debug-frame-source`, `debug-eval-in-frame`, `debug-invoke-restart`, `debug-return-from-frame`, `debug-restart-frame`
- **inspector:** `inspect`, `inspect-part`, `inspect-action`, `inspect-back`, `inspect-condition`, `inspect-frame-var`

Locators: `defun foo`, `defmethod print-object (my-class t)`, `path:0.2.1`, `line:12[:col]`, `offset:N`, `hash:H`. Read-only tools carry `readOnlyHint`; tools that write files or run code carry `destructiveHint`.

## Test

```bash
ros -e '(asdf:test-system "lisp-mcp")' -q   # includes a session against a spawned SBCL+Swank
```

## License

MIT
