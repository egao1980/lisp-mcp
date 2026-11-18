(in-package #:lisp-mcp)

;;; Live image tools: the image is a separate process reached over Swank or
;;; Slynk, so evaluation never runs inside this server.

(defparameter +connection+ '("connection" "string" nil))
(defparameter +code+ '("code" "string" nil :required t))

(defvar *backends*
  (list (cons "swank" #'repl-backend-swank:make-swank-backend)
        (cons "slynk" #'repl-backend-slynk:make-slynk-backend)))

(defun backend-named (name)
  (let ((entry (assoc (or name "swank") *backends* :test #'string-equal)))
    (unless entry
      (error 'bad-argument :message (format nil "Unknown backend ~A (swank, slynk)" name)))
    (funcall (cdr entry))))

(defun conn (args) (find-connection (arg-string args "connection")))

(defun timeout (args) (or (arg-int args "timeout") repl:*repl-timeout*))

(defun render-connection (name c)
  (let ((info (repl:repl-connection-info c)))
    (format nil "~A: ~A ~A, pid ~A, package ~A~:[ (attached)~; (spawned)~]~%" name
            (repl:connection-info-implementation info) (repl:connection-info-version info)
            (repl:connection-info-pid info) (repl:connection-info-package info)
            (repl:connection-process c))))

;;; Lifecycle

(define-tool "repl-spawn"
    (args :description "Start a fresh SBCL image with Swank or Slynk and connect to it."
          :kind :write)
    (input-schema '("backend" "string" "Default swank" :enum ("swank" "slynk"))
                  '("name" "string" nil)
                  '("command" "string" "Command prefix (default: ros +Q run --)"))
  (let* ((backend (backend-named (arg-string args "backend")))
         (command (let ((c (arg-string args "command")))
                    (if c (uiop:split-string c :separator " ") repl:*default-image-command*)))
         (c (repl:repl-spawn backend :command command :directory (state-root *state*)))
         (name (or (arg-string args "name") (fresh-connection-name (arg-string args "backend" "swank")))))
    (add-connection name c)
    (render-connection name c)))

(define-tool "repl-connect"
    (args :description "Attach to a running Swank or Slynk server." :kind :write)
    (input-schema '("port" "integer" nil :required t)
                  '("host" "string" "Default 127.0.0.1")
                  '("backend" "string" "Default swank" :enum ("swank" "slynk"))
                  '("name" "string" nil))
  (let* ((c (repl:repl-connect (backend-named (arg-string args "backend"))
                               :host (arg-string args "host" "127.0.0.1") :port (arg-int args "port")))
         (name (or (arg-string args "name") (fresh-connection-name "attached"))))
    (add-connection name c)
    (render-connection name c)))

(define-tool "repl-list" (args :description "Live connections; the default one is marked *.") (input-schema)
  (with-text (s)
    (let ((default (state-default-connection *state*)))
      (if (zerop (hash-table-count (state-connections *state*)))
          (format s "No connections. Use repl-spawn or repl-connect.~%")
          (maphash (lambda (name c)
                     (format s "~:[ ~;*~]~A" (equal name default)
                             (if (repl:repl-connected-p c) (render-connection name c)
                                 (format nil "~A: closed~%" name))))
                   (state-connections *state*))))))

(define-tool "repl-disconnect"
    (args :description "Close a connection; kill=true also stops a spawned image." :kind :write)
    (input-schema +connection+ '("kill" "boolean" "Default true"))
  (let* ((name (or (arg-string args "connection") (state-default-connection *state*)))
         (c (find-connection name)))
    (repl:repl-disconnect c :kill (arg-bool args "kill" t))
    (remove-connection name)
    (format nil "Disconnected ~A~%" name)))

;;; Evaluation

(define-tool "repl-eval"
    (args :description "Evaluate code in the image: values and output, or the debugger state."
          :kind :write)
    (input-schema +code+ +package+ +connection+
                  '("channel" "string" "Slynk channel (repl-channel-open)"))
  (let ((channel (arg-string args "channel")))
    (if channel
        (render-eval (repl:repl-channel-eval (find-channel channel) (require-arg args "code")
                                             :timeout (timeout args)))
        (render-eval (repl:repl-eval (conn args) (require-arg args "code")
                                     :package (arg-string args "package") :timeout (timeout args))))))

(define-tool "repl-channel-open"
    (args :description "Open a Slynk REPL (own thread, package, #v history) for repl-eval's channel."
          :kind :write)
    (input-schema +connection+ '("package" "string" nil) '("name" "string" nil))
  (let* ((c (conn args))
         (channel (handler-case (repl:repl-open-channel c :package (arg-string args "package"))
                    (repl:repl-not-supported ()
                      (error 'bad-argument :message "This connection has no channels; spawn or connect with backend slynk"))))
         (name (or (arg-string args "name") (fresh-channel-name))))
    (bt2:with-lock-held ((state-lock *state*))
      (setf (gethash name (state-channels *state*)) channel))
    (format nil "Channel ~A open (package ~A)~%" name (repl:repl-channel-package channel))))

(define-tool "repl-channel-close" (args :description "Close a channel opened with repl-channel-open." :kind :write)
    (input-schema '("channel" "string" nil :required t))
  (let* ((name (require-arg args "channel"))
         (channel (find-channel name)))
    (repl:repl-close-channel channel)
    (bt2:with-lock-held ((state-lock *state*))
      (remhash name (state-channels *state*)))
    (format nil "Closed ~A~%" name)))

(define-tool "repl-channel-list" (args :description "Open channels and their packages.") (input-schema)
  (with-text (s)
    (if (zerop (hash-table-count (state-channels *state*)))
        (format s "No channels~%")
        (maphash (lambda (name ch)
                   (format s "~A: package ~A~:[~; (closed)~]~%" name (repl:repl-channel-package ch)
                           (repl:repl-channel-closed-p ch)))
                 (state-channels *state*)))))

(define-tool "repl-compile"
    (args :description "Compile a form (locator) or the whole file (then loaded); notes map to file positions."
          :kind :write)
    (input-schema +path+ '("locator" "string" "Form locator; omit for the whole file") +connection+)
  (multiple-value-bind (parsed name) (read-project-file (require-arg args "path"))
    (let ((locator (arg-string args "locator"))
          (c (conn args)))
      (if locator
          (let ((node (locate-in parsed locator)))
            (render-compile (repl:repl-compile-string c (src:node-text node)
                                                      :package (src:package-at parsed (src:node-start node))
                                                      :filename (namestring (src:parsed-source-path parsed))
                                                      :position (src:node-start node))
                            :label (format nil "~A ~A" name (path-string node))))
          (progn
            (progress 0 1 "Compiling ~A" name)
            (prog1 (render-compile (repl:repl-compile-file c (src:parsed-source-path parsed) :load t)
                                   :label name)
              (progress 1 1 "Compiled ~A" name)))))))

(define-tool "repl-load-system"
    (args :description "asdf:load-system in the image." :kind :write)
    (input-schema '("system" "string" nil :required t) +connection+)
  (let ((repl:*repl-timeout* (or (arg-int args "timeout") 300))
        (system (require-arg args "system")))
    (progress 0 1 "Loading ~A" system)
    (prog1 (render-eval (repl:repl-load-system (conn args) system) :quiet t)
      (progress 1 1 "Loaded ~A" system))))

(define-tool "repl-run-tests"
    (args :description "asdf:test-system in the image." :kind :write)
    (input-schema '("system" "string" nil :required t) +connection+)
  (let ((system (require-arg args "system")))
    (progress 0 1 "Testing ~A" system)
    (prog1 (render-eval (repl:repl-eval (conn args) (format nil "(asdf:test-system ~S)" system)
                                        :timeout (or (arg-int args "timeout") 600))
                        :quiet t)
      (progress 1 1 "Tested ~A" system))))

(define-tool "repl-interrupt" (args :description "Interrupt the running evaluation (it enters the debugger)." :kind :write)
    (input-schema +connection+)
  (repl:repl-interrupt (conn args))
  (format nil "Interrupt sent; the pending repl-eval returns the debugger state.~%"))

(define-tool "repl-output" (args :description "Image output outside eval results (other threads); clears it.")
    (input-schema +connection+)
  (let ((text (repl:repl-output (conn args) :clear t)))
    (if (plusp (length text)) text (format nil "(no output)~%"))))

;;; Introspection

(define-tool "repl-describe" (args :description "DESCRIBE a symbol in the image.") (input-schema +symbol+ +package+ +connection+)
  (repl:repl-describe (conn args) (require-arg args "symbol") :package (arg-string args "package")))

(define-tool "repl-documentation" (args :description "Documentation string of a symbol in the image.") (input-schema +symbol+ +package+ +connection+)
  (or (repl:repl-documentation (conn args) (require-arg args "symbol") :package (arg-string args "package"))
      "No documentation"))

(define-tool "repl-arglist" (args :description "Lambda list of a function or macro in the image.") (input-schema +symbol+ +package+ +connection+)
  (or (repl:repl-arglist (conn args) (require-arg args "symbol") :package (arg-string args "package"))
      "No arglist"))

(define-tool "repl-apropos" (args :description "Symbols matching a pattern in the image.")
    (input-schema '("pattern" "string" nil :required t) +package+ '("external_only" "boolean" nil) +connection+)
  (format nil "~{~A~%~}" (repl:repl-apropos (conn args) (require-arg args "pattern")
                                            :package (arg-string args "package")
                                            :external-only (arg-bool args "external_only"))))

(define-tool "repl-complete" (args :description "Symbol completions for a prefix in the image.")
    (input-schema '("prefix" "string" nil :required t) +package+ +connection+)
  (format nil "~{~A~%~}" (repl:repl-completions (conn args) (require-arg args "prefix") :package (arg-string args "package"))))

(define-tool "repl-macroexpand" (args :description "Macroexpand a form in the image (all=true for full expansion).")
    (input-schema +code+ +package+ '("all" "boolean" nil) +connection+)
  (repl:repl-macroexpand (conn args) (require-arg args "code") :package (arg-string args "package")
                                                               :all (arg-bool args "all")))

(define-tool "repl-xref" (args :description "Who calls, references, binds, sets, macroexpands or specializes a symbol (image).")
    (input-schema '("type" "string" nil :required t :enum ("calls" "references" "binds" "sets" "macroexpands" "specializes"))
                  +symbol+ +package+ +connection+)
  (let ((type (find (require-arg args "type") '(:calls :references :binds :sets :macroexpands :specializes)
                    :key #'symbol-name :test #'string-equal)))
    (unless type (error 'bad-argument :message "Unknown xref type"))
    (render-xrefs (repl:repl-xref (conn args) type (require-arg args "symbol") :package (arg-string args "package")))))

(define-tool "repl-find-definitions" (args :description "Source locations of a symbol's definitions, from the image.")
    (input-schema +symbol+ +package+ +connection+)
  (render-xrefs (repl:repl-find-definitions (conn args) (require-arg args "symbol") :package (arg-string args "package"))))

(define-tool "repl-threads" (args :description "Threads in the image.") (input-schema +connection+)
  (with-text (s)
    (dolist (th (repl:repl-list-threads (conn args)))
      (format s "~D: ~A [~A] ~A~%" (repl:thread-info-index th) (repl:thread-info-name th)
              (repl:thread-info-id th) (repl:thread-info-status th)))))

;;; Debugger

(define-tool "debug-state" (args :description "The innermost debugger level, if any.") (input-schema +connection+)
  (let ((state (repl:repl-debugger-state (conn args))))
    (if state (render-debugger state) (format nil "Not in the debugger~%"))))

(define-tool "debug-backtrace" (args :description "Backtrace frames of the innermost debugger level.")
    (input-schema '("start" "integer" "Default 0") '("end" "integer" "Default 40") +connection+)
  (with-text (s)
    (dolist (f (repl:repl-backtrace (conn args) :start (arg-int args "start" 0) :end (arg-int args "end" 40)))
      (format s "~D: ~A~:[~; (restartable)~]~%" (repl:frame-info-index f) (repl:frame-info-description f)
              (repl:frame-info-restartable-p f)))))

(define-tool "debug-frame-locals" (args :description "Local variables of a frame.")
    (input-schema '("frame" "integer" nil :required t) +connection+)
  (let ((locals (repl:repl-frame-locals (conn args) (arg-int args "frame"))))
    (if locals
        (with-text (s)
          (loop for l in locals for i from 0
                do (format s "~D: ~A = ~A~%" i (repl:frame-local-name l) (repl:frame-local-value l))))
        (format nil "No locals (compile with (debug 3) for more)~%"))))

(define-tool "debug-frame-source" (args :description "Source location of a frame.")
    (input-schema '("frame" "integer" nil :required t) +connection+)
  (render-location (repl:repl-frame-source (conn args) (arg-int args "frame"))))

(define-tool "debug-eval-in-frame" (args :description "Evaluate code in a frame's lexical environment." :kind :write)
    (input-schema '("frame" "integer" nil :required t) +code+ +package+ +connection+)
  (let ((v (repl:repl-eval-in-frame (conn args) (arg-int args "frame") (require-arg args "code")
                                    :package (arg-string args "package"))))
    (if (typep v 'repl:debugger-state)
        (render-debugger v)
        ;; Swank's pprint result starts and ends with newlines
        (format nil "=> ~A~%" (string-trim '(#\Newline #\Space) (princ-to-string v))))))

(define-tool "debug-invoke-restart"
    (args :description "Invoke a restart by index; returns values, abort or a new debugger level."
          :kind :write)
    (input-schema '("restart" "integer" nil :required t) +connection+)
  (handler-case (render-eval (repl:repl-invoke-restart (conn args) (arg-int args "restart")))
    (repl:repl-invalid-restart (e)
      (error 'bad-argument :message (princ-to-string e)))))

(define-tool "debug-step"
    (args :description "Step from a frame: into, next or out (code with (debug 3), entered with step)."
          :kind :write)
    (input-schema '("frame" "integer" "Default 0")
                  '("mode" "string" "Default into" :enum ("into" "next" "out")) +connection+)
  (render-eval (repl:repl-step (conn args) (arg-int args "frame" 0)
                               :mode (let ((m (arg-string args "mode" "into")))
                                       (cond ((string-equal m "next") :next)
                                             ((string-equal m "out") :out)
                                             (t :into))))))

(define-tool "inspect-history"
    (args :description "Inspect #v<entry>:<index> of a channel's history."
          :kind :write)
    (input-schema '("channel" "string" nil :required t) '("entry" "integer" nil :required t)
                  '("index" "integer" "Default 0"))
  (render-inspector (repl:repl-channel-inspect (find-channel (require-arg args "channel"))
                                               (arg-int args "entry") (arg-int args "index" 0))))

(define-tool "debug-return-from-frame" (args :description "Return a value (code) from a frame." :kind :write)
    (input-schema '("frame" "integer" nil :required t) +code+ +connection+)
  (format nil "~A~%" (repl:repl-return-from-frame (conn args) (arg-int args "frame") (require-arg args "code"))))

(define-tool "debug-restart-frame" (args :description "Restart execution of a (restartable) frame." :kind :write)
    (input-schema '("frame" "integer" nil :required t) +connection+)
  (format nil "~A~%" (repl:repl-restart-frame (conn args) (arg-int args "frame"))))

;;; Inspector

(define-tool "inspect"
    (args :description "Inspect a value: code to evaluate, or a result id from repl-eval."
          :kind :write)
    (input-schema '("code" "string" "Expression, or:")
                  '("result" "integer" "repl-eval result id")
                  '("index" "integer" "Value of it (default 0)") +package+ +connection+)
  (let ((c (conn args)))
    (render-inspector
     (cond ((arg-int args "result")
            (repl:repl-inspect-result c (make-instance 'repl:eval-result :id (arg-int args "result"))
                                      (arg-int args "index" 0)))
           ((arg-string args "code") (repl:repl-inspect c (arg-string args "code") :package (arg-string args "package")))
           (t (error 'bad-argument :message "Give code or result"))))))

(define-tool "inspect-part" (args :description "Inspect part n of the current inspector view.")
    (input-schema '("index" "integer" nil :required t) +connection+)
  (render-inspector (repl:repl-inspect-part (conn args) (arg-int args "index"))))

(define-tool "inspect-action" (args :description "Run action n of the current inspector view." :kind :write)
    (input-schema '("index" "integer" nil :required t) +connection+)
  (render-inspector (repl:repl-inspector-action (conn args) (arg-int args "index"))))

(define-tool "inspect-back" (args :description "Back to the previous inspector view.") (input-schema +connection+)
  (let ((v (repl:repl-inspector-pop (conn args))))
    (if v (render-inspector v) (format nil "At the first view~%"))))

(define-tool "inspect-condition" (args :description "Inspect the condition of the innermost debugger level.") (input-schema +connection+)
  (render-inspector (repl:repl-inspect-condition (conn args))))

(define-tool "inspect-frame-var" (args :description "Inspect a frame local (var: index from debug-frame-locals).")
    (input-schema '("frame" "integer" nil :required t) '("var" "integer" nil :required t) +connection+)
  (render-inspector (repl:repl-inspect-frame-var (conn args) (arg-int args "frame") (arg-int args "var"))))
