(in-package #:lisp-mcp)

(defparameter *instructions*
  "Common Lisp tools with three layers.
SOURCE (read only, never evaluates): source-outline → source-read (form text + file hash) → source-check (reader errors, paren imbalances with fix positions, lint) → source-find-definitions / source-find-references / source-packages. live=true merges answers from the connected image.
EDIT (structural, never unbalanced): edit-replace / edit-insert / edit-delete / edit-wrap / edit-splice / edit-raise / edit-slurp / edit-barf / edit-transpose / edit-move / edit-rename-symbol / edit-add-export / edit-repair / edit-new-file. Address forms with a locator (\"defun foo\", \"path:0.2\", \"line:12\"); pass expected_hash from source-read; dry_run shows the diff. Inserted code is re-indented. Do not write .lisp files any other way.
LIVE IMAGE (Swank/Slynk, separate process): repl-spawn or repl-connect first, then repl-eval / repl-compile (notes mapped to file positions) / repl-load-system / repl-run-tests. An error returns the debugger state instead of hanging: use debug-backtrace, debug-frame-locals, debug-eval-in-frame, debug-step, inspect-condition, then debug-invoke-restart. inspect / inspect-part walk values (repl-eval prints a result id). With Slynk, repl-channel-open gives extra REPLs with #v value history.
RESOURCES: lisp-source:///{path} (outline), repl://{connection}/output and repl://{connection}/debugger (subscribe for updates), repl://connections. Image output and debugger entries are also sent as log messages.")

(defun resource-text (thunk)
  "Run THUNK for a resource read; this server's errors become MCP errors."
  (handler-case (funcall thunk)
    (lisp-mcp-error (e)
      (error 'mcp:mcp-error :message (princ-to-string e) :code -32602))))

(defun complete-prefix (candidates value)
  (let ((matches (remove-if-not (lambda (c) (and (>= (length c) (length value))
                                                 (string= value c :end2 (length value))))
                                candidates)))
    (subseq matches 0 (min 100 (length matches)))))

(defun register-resources (server)
  (mcp:register-resource-template
   server
   (mcp:make-mcp-resource-template
    "lisp-source:///{+path}" :name "lisp-source" :mime-type "text/plain"
    :description "Outline of a project file (definitions, lines, structural paths, file hash)."
    :complete (lambda (name value)
                (declare (ignore name))
                (complete-prefix (mapcar #'relative-name (project-files)) value))
    :handler (lambda (uri vars)
               (declare (ignore uri))
               (resource-text (lambda ()
                                (multiple-value-bind (parsed name)
                                    (read-project-file (cdr (assoc "path" vars :test #'string=)))
                                  (render-outline parsed name)))))))
  (flet ((connection-names (name value)
           (declare (ignore name))
           (complete-prefix (sort (alexandria:hash-table-keys (state-connections *state*)) #'string<) value)))
    (mcp:register-resource-template
     server
     (mcp:make-mcp-resource-template
      "repl://{connection}/output" :name "repl-output" :mime-type "text/plain"
      :description "Output an image wrote outside eval results. Subscribe to be told when it grows."
      :complete #'connection-names
      :handler (lambda (uri vars)
                 (declare (ignore uri))
                 (resource-text (lambda ()
                                  (repl:repl-output (find-connection (cdr (assoc "connection" vars :test #'string=)))))))))
    (mcp:register-resource-template
     server
     (mcp:make-mcp-resource-template
      "repl://{connection}/debugger" :name "repl-debugger" :mime-type "text/plain"
      :description "The innermost debugger level of an image, or a note that it is not in the debugger. Subscribe to be told when it changes."
      :complete #'connection-names
      :handler (lambda (uri vars)
                 (declare (ignore uri))
                 (resource-text (lambda ()
                                  (let ((state (repl:repl-debugger-state
                                                (find-connection (cdr (assoc "connection" vars :test #'string=))))))
                                    (if state (render-debugger state) (format nil "Not in the debugger~%")))))))))
  (mcp:register-resource
   server
   (mcp:make-mcp-resource
    "repl://connections" :name "repl-connections" :mime-type "text/plain"
    :description "Live image connections."
    :handler (lambda (resource)
               (declare (ignore resource))
               (call-tool-text "repl-list" (mcp:json-object))))))

(defun call-tool-text (name args)
  (let ((tool (find name *tools* :key #'first :test #'string=)))
    (funcall (fifth tool) args)))

(defun make-lisp-mcp-server (&key (root (uiop:getcwd)) (name "lisp-mcp") (version "0.2.0"))
  "MCP server with every lisp-mcp tool and resource, rooted at ROOT. Binds
*STATE* and *SERVER*."
  (setf *state* (make-state :root root))
  (process-backend-uiop:use-uiop-process-backend)
  (let ((server (make-instance 'mcp:mcp-server :name name :version version)))
    (setf (mcp:mcp-server-instructions server) *instructions*
          *server* server)
    (dolist (tool (reverse *tools*))
      (destructuring-bind (tool-name description schema annotations handler) tool
        (mcp:register-tool server
                           (mcp:make-mcp-tool tool-name :description description :input-schema schema
                                                        :annotations annotations :handler handler))))
    (register-resources server)
    server))
