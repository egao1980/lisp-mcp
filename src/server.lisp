(in-package #:lisp-mcp)

(defparameter *instructions*
  "Common Lisp tools. SOURCE (static, never evaluates): source-outline, then source-read (form + file hash), source-check, source-find-*.
EDIT (structural, never unbalanced; the only way to write .lisp): edit-*. Locator \"defun foo\" / \"path:0.2\" / \"line:12\"; pass expected_hash; dry_run shows the diff.
LIVE (Swank/Slynk image): repl-spawn or repl-connect, then repl-eval / repl-compile / repl-load-system / repl-run-tests. An error returns the debugger state: debug-backtrace, debug-frame-locals, inspect-condition, debug-invoke-restart.
JOBS: a call that outlasts its timeout keeps running as job jN. Do not start it again; wait for the log message or call job-status / job-wait; job-cancel interrupts it.
LONG OUTPUT: a cut-short result names a stored id oN; read it with output-read (offset, grep) instead of re-running.
Resources: lisp-source:///{path}, repl://{connection}/output and /debugger (subscribable), lisp-output://{id}.")

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

(defun make-lisp-mcp-server (&key (root (uiop:getcwd)) (name "lisp-mcp") (version "0.4.0"))
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
                           (mcp:make-mcp-tool tool-name :description description
                                                        :input-schema (add-job-argument tool-name (add-verbose-argument tool-name schema))
                                                        :annotations annotations
                                                        :handler (bounded-handler tool-name (if (job-tool-p tool-name) (job-handler tool-name handler) handler))))))
    (register-resources server)
    (register-output-resources server)
    server))
