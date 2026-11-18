(in-package #:lisp-mcp)

(defparameter *instruction-parts*
  '((:source "SOURCE (read only, never evaluates): source-outline, source-read (form + file hash), source-check (reader errors, imbalances with fix, lint), source-find-*.")
    (:source-image " live=true adds the image's answer.")
    (:edit "
EDIT (structural, never unbalanced; the only way to write .lisp): edit-replace / -insert / -delete; edit-structure op wrap (head) | splice | raise | slurp / barf (direction) | transpose | move (target_locator, target_path) | repair (index); edit-rename-symbol, edit-add-export, edit-new-file. Locator: \"defun foo\", \"defmethod m (c t)\", \"path:0.2.1\" (child indices), \"line:12[:col]\", \"offset:N\", \"hash:H\". Pass expected_hash from source-read (a stale one shows the current form and hash); dry_run shows the diff. Code is re-indented.")
    (:edit-image " compile=true appends the image's notes.")
    (:repl "
LIVE (Swank/Slynk image, separate process): repl-spawn or repl-connect, then repl-eval / repl-compile / repl-load-system / repl-run-tests.")
    (:debug "
DEBUG: an error returns the debugger state: debug-backtrace, debug-frame-locals, debug-eval-in-frame, debug-step, then debug-invoke-restart.")
    (:inspect "
INSPECT: inspect (code or result id), inspect-part.")
    (:jobs "
JOBS: a call past its timeout (seconds) keeps running as job jN: do not start it again; wait for the log message, job-status or job-wait; job-cancel interrupts it.")
    (:output "
LONG OUTPUT: a cut-short result names a stored id oN: read it with output-read (offset, grep), do not re-run."))
  "Server instruction text by group; :source-image and :edit-image are added
when an image group is on too.")

(defparameter *argument-notes*
  '(((:source :edit :repl) "path is relative to the root")
    ((:source :edit :repl) "symbol may be pkg:name, package is for an unqualified one")
    ((:repl :debug :inspect :jobs) "connection names an image (default: the latest)")
    ((:edit :repl :debug :inspect) "verbose=true gives full detail"))
  "Arguments that carry no description, explained once: (groups text).")

(defun server-instructions (&key (state *state*))
  "Instructions for the groups STATE offers: their parts and their resources."
  (let* ((groups (state-tool-groups state))
         (image (image-tools-p :state state))
         (keys (append groups
                       (and image (member :source groups) '(:source-image))
                       (and image (member :edit groups) '(:edit-image))))
         (resources (append (and (member :source groups) '("lisp-source:///{path}"))
                            (and image '("repl://{connection}/output and /debugger (subscribable)" "repl://connections"))
                            (and (member :output groups) '("lisp-output://{id}")))))
    (string-trim '(#\Newline)
                 (with-output-to-string (s)
                   (loop for (key text) in *instruction-parts*
                         when (member key keys) do (write-string text s))
                   (let ((notes (loop for (on text) in *argument-notes*
                                      when (intersection on groups) collect text)))
                     (when notes
                       (format s "~%ARGS: ~{~A~^; ~}." notes)))
                   (when resources
                     (format s "~%Resources: ~{~A~^, ~}." resources))))))

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
  "Resources of the groups this server offers."
  (when (group-on-p :source)
    (register-source-resources server))
  (when (image-tools-p)
    (register-image-resources server))
  (when (group-on-p :output)
    (register-output-resources server)))

(defun register-source-resources (server)
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
                                  (render-outline parsed name))))))))

(defun register-image-resources (server)
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

(defparameter *image-arguments* '("live" "compile" "connection")
  "Arguments of source and edit tools that only matter with a live image.")

(defun tool-schema (tool-name schema)
  "SCHEMA as this server offers it: verbose and timeout where they apply,
without the image arguments of a source or edit tool when no image group is on."
  (let ((schema (add-job-argument tool-name (add-verbose-argument tool-name schema))))
    (if (and (member (tool-group tool-name) '(:source :edit)) (not (image-tools-p)))
        (schema-without schema *image-arguments*)
        schema)))

(defun make-lisp-mcp-server (&key (root (uiop:getcwd)) (name "lisp-mcp") (version "0.5.0") tools)
  "MCP server rooted at ROOT with the tools and resources of the groups TOOLS
selects (see PARSE-TOOL-GROUPS; default all). Binds *STATE* and *SERVER*."
  (setf *state* (make-state :root root :tool-groups tools))
  (process-backend-uiop:use-uiop-process-backend)
  (let ((server (make-instance 'mcp:mcp-server :name name :version version)))
    (setf (mcp:mcp-server-instructions server) (server-instructions)
          *server* server)
    (dolist (tool (reverse *tools*))
      (destructuring-bind (tool-name description schema annotations handler) tool
        (when (group-on-p (tool-group tool-name))
          (mcp:register-tool server
                             (mcp:make-mcp-tool tool-name :description description
                                                          :input-schema (tool-schema tool-name schema)
                                                          :annotations annotations
                                                          :handler (bounded-handler tool-name (if (job-tool-p tool-name) (job-handler tool-name handler) handler)))))))
    (register-resources server)
    server))
