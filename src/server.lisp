(in-package #:lisp-mcp)

(defparameter *instructions*
  "Common Lisp tools with three layers.
SOURCE (read only, never evaluates): source-outline → source-read (form text + file hash) → source-check (reader errors, paren imbalances with fix positions, lint) → source-find-definitions / source-find-references.
EDIT (structural, never unbalanced): edit-replace / edit-insert / edit-delete / edit-wrap / edit-splice / edit-raise / edit-slurp / edit-barf / edit-transpose / edit-rename-symbol / edit-add-export / edit-repair / edit-new-file. Address forms with a locator (\"defun foo\", \"path:0.2\", \"line:12\"); pass expected_hash from source-read; dry_run shows the diff. Do not write .lisp files any other way.
LIVE IMAGE (Swank/Slynk, separate process): repl-spawn or repl-connect first, then repl-eval / repl-compile (notes mapped to file positions) / repl-load-system / repl-run-tests. An error returns the debugger state instead of hanging: use debug-backtrace, debug-frame-locals, debug-eval-in-frame, inspect-condition, then debug-invoke-restart. inspect / inspect-part walk values (repl-eval prints a result id).")

(defun make-lisp-mcp-server (&key (root (uiop:getcwd)) (name "lisp-mcp") (version "0.1.0"))
  "MCP server with every lisp-mcp tool, rooted at ROOT. Binds *STATE*."
  (setf *state* (make-state :root root))
  (process-backend-uiop:use-uiop-process-backend)
  (let ((server (make-instance 'mcp:mcp-server :name name :version version)))
    (setf (mcp:mcp-server-instructions server) *instructions*)
    (dolist (tool (reverse *tools*) server)
      (destructuring-bind (tool-name description schema annotations handler) tool
        (mcp:register-tool server
                           (mcp:make-mcp-tool tool-name :description description :input-schema schema
                                                        :annotations annotations :handler handler))))))
