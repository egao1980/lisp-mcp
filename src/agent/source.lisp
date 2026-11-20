(in-package #:lisp-mcp/agent)

;;; LISP-TOOLS (H4a): lisp-mcp's tools as an ai-agent-protocol tool source.
;;; A call runs the same handler the MCP server registers (with the same
;;; output bounding and background jobs), in-process, with LISP-MCP:*STATE*
;;; bound to the source's own state: no JSON-RPC, no second process. Tool
;;; names, schemas, descriptions and result texts are lisp-mcp's.
;;;
;;; Permission resources: code-taking tools scan their code with
;;; LISP-MCP:FORM-RESOURCES; path tools give "path:<relative>" (as
;;; agent-tools' file tools do); load/test give "system:<name>";
;;; debug-invoke-restart gives "restart:<name>". :OBSERVE mode lists only
;;; read tools plus the code tools, and refuses code that scans outside
;;; "lisp:pure" / "lisp:reads *" (plus OBSERVE-ALLOW globs).
;;;
;;; The class, its readers and the constants are the H4a contract.

(defparameter +code-tools+ '("repl-eval" "debug-eval-in-frame" "inspect")
  "Tools whose `code` argument is scanned; without :NEEDS-APPROVAL NIL they ask
only when the scan is not observe-safe (inspect: only when it has code).")

(defparameter +approval-tools+
  '("repl-spawn" "repl-connect" "repl-compile" "repl-load-system" "repl-run-tests"
    "debug-invoke-restart" "debug-step" "debug-restart-frame" "debug-return-from-frame"
    "inspect-action")
  "Tools that always ask when :NEEDS-APPROVAL is true and no rule decides.")

(defparameter +observe-tools+
  '("repl-eval" "debug-eval-in-frame" "inspect" "repl-connect" "repl-disconnect")
  "Write-kind tools an :OBSERVE source still offers, besides every read-kind
tool (lisp-mcp's readOnlyHint). The code tools among them are scan-gated.")

(defclass lisp-tools ()
  ((root :initarg :root :reader lisp-tools-root
         :documentation "Project root directory (a pathname, its truename).")
   (groups :initarg :groups :reader lisp-tools-groups
           :documentation "lisp-mcp tool group keywords, in catalog order.")
   (names :initarg :names :reader lisp-tools-names
          :documentation "Tool names this source offers, in lisp-mcp catalog order.")
   (mode :initarg :mode :initform :full :reader lisp-tools-mode
         :type (member :full :observe))
   (observe-allow :initarg :observe-allow :initform '() :reader lisp-tools-observe-allow
                  :documentation "Extra resource globs :OBSERVE mode accepts.")
   (needs-approval :initarg :needs-approval :initform t :reader lisp-tools-needs-approval)
   (package :initarg :package :initform "cl-user" :reader lisp-tools-package
            :documentation "Package for scanning code that names none.")
   (state :initarg :state :reader lisp-tools-state
          :documentation "The LISP-MCP::LISP-MCP-STATE every call binds.")
   (closed :initform nil :accessor %closed :reader lisp-tools-closed-p))
  (:documentation "lisp-mcp's source, edit, output, repl, debug, inspect and
job tools as an ai-agent-protocol tool source."))

(defun lisp-tools-p (x) (typep x 'lisp-tools))

;;; Catalog

(defun %entry (name)
  "lisp-mcp's registry entry (name description schema annotations handler)."
  (find name lm::*tools* :key #'first :test #'equal))

(defun %catalog (state)
  "Tool names of STATE's groups, in lisp-mcp registration order."
  (loop for entry in (reverse lm::*tools*)
        for name = (first entry)
        when (lm::group-on-p (lm::tool-group name) :state state) collect name))

(defun %observe-kind-p (name)
  "A tool :OBSERVE mode offers: read kind (readOnlyHint) or +OBSERVE-TOOLS+."
  (or (and (fourth (%entry name)) t)
      (and (member name +observe-tools+ :test #'string=) t)))

(defun make-lisp-tools (&key (root (uiop:getcwd)) groups tools (mode :full) observe-allow
                             (needs-approval t) (package "cl-user"))
  "A LISP-TOOLS rooted at ROOT (a directory that must exist). GROUPS selects
lisp-mcp tool groups like its --tools (NIL = all, a keyword list, or a comma
string with presets \"code\" / \"image\" / \"all\"). TOOLS, when given, keeps
only those names. MODE is :FULL or :OBSERVE. Unknown groups, tool names or
modes → ERROR."
  (check-type mode (member :full :observe))
  (let ((dir (probe-file (uiop:ensure-directory-pathname root))))
    (unless (and dir (uiop:directory-pathname-p dir))
      (error 'lm::bad-argument
             :message (format nil "lisp-tools root ~A is not an existing directory" root)))
    (let* ((state (lm::make-state :root dir :tool-groups groups)) ; unknown group → BAD-ARGUMENT
           (catalog (%catalog state)))
      (dolist (name tools)
        (unless (member name catalog :test #'equal)
          (error 'lm::bad-argument
                 :message (format nil "Unknown tool ~S for groups ~{~(~A~)~^, ~}"
                                  name (lm::state-tool-groups state)))))
      ;; repl-spawn starts images through process-protocol, as the MCP server does
      (process-backend-uiop:use-uiop-process-backend)
      (make-instance 'lisp-tools
                     :root (lm:state-root state)
                     :groups (lm::state-tool-groups state)
                     :names (remove-if-not (lambda (name)
                                             (and (or (null tools) (member name tools :test #'equal))
                                                  (or (eq mode :full) (%observe-kind-p name))))
                                           catalog)
                     :mode mode
                     :observe-allow observe-allow
                     :needs-approval needs-approval
                     :package package
                     :state state))))

(defun %offered-p (tools name)
  (and (member name (lisp-tools-names tools) :test #'equal) t))

;;; Arguments

(defun %blank-p (string)
  (or (null string) (every (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) string)))

(defun %decode-arguments (arguments)
  "ARGUMENTS (a JSON object string or a string-keyed hash table) → an EQUAL
hash table. NIL or a blank string count as {}."
  (let ((value (typecase arguments
                 (hash-table arguments)
                 (null (make-hash-table :test #'equal))
                 (string
                  (if (%blank-p arguments)
                      (make-hash-table :test #'equal)
                      (handler-case (json:decode arguments)
                        (json:json-error (c)
                          (error 'agent:tool-input-error
                                 :message (format nil "arguments are not valid JSON (~A); send one JSON object with named keys" c))))))
                 (t arguments))))
    (unless (hash-table-p value)
      (error 'agent:tool-input-error :message "arguments must be a JSON object with named keys"))
    (if (eq (hash-table-test value) 'equal)
        value
        (let ((copy (make-hash-table :test #'equal)))
          (maphash (lambda (k v) (setf (gethash (string k) copy) v)) value)
          copy))))

;;; Permission resources

(defun %code-resources (tools args)
  "FORM-RESOURCES of the `code` argument, in the call's package or the
source's; (\"*\") for missing or blank code."
  (let ((code (lm::arg-string args "code")))
    (if (%blank-p code)
        (list "*")
        (or (lm:form-resources code :package (or (lm::arg-string args "package")
                                                 (lisp-tools-package tools)))
            (list "*")))))

(defun %absolute-path-p (path)
  (or (and (plusp (length path)) (char= (char path 0) #\/))
      (and (> (length path) 1) (char= (char path 1) #\:))))

(defun %normalize-relative (path)
  "PATH's parts with . and empty parts dropped and .. applied, or NIL when it
climbs out."
  (let ((stack '()))
    (dolist (part (uiop:split-string path :separator "/") (format nil "~{~A~^/~}" (reverse stack)))
      (cond ((member part '("" ".") :test #'string=))
            ((string= part "..") (if stack (pop stack) (return nil)))
            (t (push part stack))))))

(defun %path-resource (tools path)
  "\"path:<rel>\": lexical, relative to the root, or PATH as given when it
climbs out of it (agent-tools' convention)."
  (let* ((p (substitute #\/ #\\ path))
         (root (substitute #\/ #\\ (namestring (lisp-tools-root tools))))
         (rel (cond ((not (%absolute-path-p p)) (%normalize-relative p))
                    ((and (> (length p) (length root)) (string= root p :end2 (length root)))
                     (%normalize-relative (subseq p (length root))))
                    ((string= (string-right-trim "/" p) (string-right-trim "/" root)) "")
                    (t nil))))
    (concatenate 'string "path:" (if (and rel (string/= rel "")) rel (if rel "." path)))))

(defun %compile-resources (tools args)
  "repl-compile: the file, then the scan of the located form in the package in
effect at it, or of the whole file."
  (let ((path (lm::arg-string args "path")))
    (if (%blank-p path)
        (list "*")
        (handler-case
            (let ((lm:*state* (lisp-tools-state tools)))
              (let ((parsed (lm::read-project-file path))
                    (locator (lm::arg-string args "locator")))
                (cons (%path-resource tools path)
                      (if (%blank-p locator)
                          (lm:form-resources (src:source-string (src:parsed-source-text parsed)))
                          (let ((node (lm::locate-in parsed locator)))
                            (lm:form-resources (src:node-text node)
                                               :package (src:package-at parsed (src:node-start node))))))))
          ((or lm::lisp-mcp-error src:lisp-source-error) ()
            (list (concatenate 'string "path:" path) "lisp:*"))))))

(defun %restart-name (name)
  "restart-info-name downcased, without a package prefix or a leading *."
  (let* ((s (string-downcase (princ-to-string name)))
         (colon (position #\: s :from-end t))
         (s (if colon (subseq s (1+ colon)) s)))
    (if (and (plusp (length s)) (char= (char s 0) #\*)) (subseq s 1) s)))

(defun %restart-resources (tools args)
  (let* ((state (lisp-tools-state tools))
         (index (lm::arg-int args "restart"))
         (conn (handler-case (lm::find-connection (lm::arg-string args "connection") state)
                 (lm::lisp-mcp-error () nil)))
         (debugger (and conn index (repl:repl-debugger-state conn)))
         (restarts (and debugger (repl:debugger-state-restarts debugger)))
         (restart (and restarts (>= index 0)
                       (or (find index restarts :key #'repl:restart-info-index)
                           (nth index restarts)))))
    (if restart
        (list (concatenate 'string "restart:" (%restart-name (repl:restart-info-name restart))))
        (list "*"))))

(defun %prefix-p (prefix name)
  (alexandria:starts-with-subseq prefix name))

(defun %resources (tools name args)
  "Resources of tool NAME with decoded ARGS (see the brief's table)."
  (flet ((path-of (key)
           (let ((p (lm::arg-string args key)))
             (and (not (%blank-p p)) (%path-resource tools p)))))
    (cond ((member name '("repl-eval" "debug-eval-in-frame" "debug-return-from-frame") :test #'string=)
           (%code-resources tools args))
          ((string= name "inspect")
           (if (%blank-p (lm::arg-string args "code"))
               (let ((result (lm::arg-int args "result")))
                 (if result (list (format nil "inspect:~D" result)) (list "*")))
               (%code-resources tools args)))
          ((string= name "repl-compile") (%compile-resources tools args))
          ((%prefix-p "edit-" name)
           (let ((paths (remove nil (list (path-of "path") (path-of "target_path")))))
             (append (or paths (list "*"))
                     (and (lm::arg-bool args "compile") (%code-resources tools args)))))
          ((%prefix-p "source-" name)
           (let ((p (path-of "path"))) (if p (list p) (list "*"))))
          ((member name '("repl-load-system" "repl-run-tests") :test #'string=)
           (let ((system (lm::arg-string args "system")))
             (if (%blank-p system)
                 (list "*")
                 (list (concatenate 'string "system:" (string-downcase system))))))
          ((string= name "debug-invoke-restart") (%restart-resources tools args))
          ((string= name "repl-spawn") (list "image:spawn"))
          ((string= name "repl-connect")
           (let ((port (lm::arg-int args "port")))
             (if port
                 (list (format nil "image:~A:~D" (lm::arg-string args "host" "127.0.0.1") port))
                 (list "*"))))
          (t (list "*")))))

(defun observe-safe-resources-p (resources &optional allow)
  "True when every one of RESOURCES is \"lisp:pure\", matches \"lisp:reads *\",
or matches one of the globs ALLOW (ai-agent-protocol glob syntax)."
  (every (lambda (r)
           (or (string= r "lisp:pure")
               (agent:permission-glob-match-p "lisp:reads *" r)
               (some (lambda (glob) (agent:permission-glob-match-p glob r)) allow)))
         resources))

;;; Observe mode

(defparameter +observe-feedback+
  "This source is in observe mode: it runs only read tools and code that scans to lisp:pure or lisp:reads (plus the source's observe-allow globs). Inspect the image without changing it; ask the user to run anything else."
  "Feedback of an OBSERVE-MODE-REFUSED.")

(defun %refuse (name resources)
  "Signal OBSERVE-MODE-REFUSED for tool NAME with the offending RESOURCES."
  (error 'observe-mode-refused
         :action name
         :resources resources
         :call-resources resources
         :feedback +observe-feedback+
         :message (format nil "~A is refused in observe mode~@[ (~{~A~^, ~})~]; observe mode runs only read tools and lisp:pure / lisp:reads code"
                          name resources)))

(defun %observe-refused-tool-p (tools name)
  "NAME is a tool of the source's full catalog that :OBSERVE mode leaves out."
  (and (eq (lisp-tools-mode tools) :observe)
       (not (%offered-p tools name))
       (member name (%catalog (lisp-tools-state tools)) :test #'equal)
       t))

(defun %observe-gate (tools name args)
  "In :OBSERVE mode, refuse a code tool whose scan is not observe-safe, with
only the offending resources. Runs before the handler."
  (when (and (eq (lisp-tools-mode tools) :observe)
             (member name +code-tools+ :test #'string=)
             (not (and (string= name "inspect") (%blank-p (lm::arg-string args "code")))))
    (let* ((allow (lisp-tools-observe-allow tools))
           (offending (remove-if (lambda (r) (observe-safe-resources-p (list r) allow))
                                 (%code-resources tools args))))
      (when offending (%refuse name offending)))))

;;; Calls

(defun %safe-resources (tools name arguments)
  "TOOL-PERMISSION-RESOURCES' value: never signals."
  ;; Rules must not crash a run: arguments that do not parse, or a failed
  ;; lookup, give the catch-all resource.
  (handler-case (%resources tools name (%decode-arguments arguments))
    (error () (list "*"))))

(defun call-lisp-tool (tools name arguments)
  "Run tool NAME with ARGUMENTS (a JSON object string or a string-keyed hash
table) synchronously → its result string. Any call after CLOSE-LISP-TOOLS →
TOOL-UNAVAILABLE; a name not offered → AGENT-UNKNOWN-TOOL (in :OBSERVE mode a
tool of the full catalog → OBSERVE-MODE-REFUSED); code an observe source does
not run → OBSERVE-MODE-REFUSED, before the handler; arguments that are not a
JSON object, and lisp-mcp's argument / path / locator / edit / no-connection
/ unknown-connection errors → TOOL-INPUT-ERROR (message kept). Other
conditions (wire errors) pass through."
  (when (lisp-tools-closed-p tools)
    (error 'agent:tool-unavailable
           :message (format nil "~A: this lisp-tools source is closed" name)))
  (let ((state (lisp-tools-state tools)))
    (unless (%offered-p tools name)
      (if (%observe-refused-tool-p tools name)
          (%refuse name (%safe-resources tools name arguments))
          (error 'agent:agent-unknown-tool
                 :name name
                 :message (format nil "lisp-tools offers no tool ~S" name))))
    (let ((args (%decode-arguments arguments)))
      (%observe-gate tools name args)
      (let* ((handler (fifth (%entry name)))
             (wrapped (lm::bounded-handler name (if (lm::job-tool-p name)
                                                    (lm::job-handler name handler)
                                                    handler))))
        (handler-case (let ((lm:*state* state)
                            (lm::*server* nil))
                        (funcall wrapped args))
          ((or lm::lisp-mcp-error src:lisp-source-error) (c)
            (error 'agent:tool-input-error :message (princ-to-string c))))))))

(defun close-lisp-tools (tools)
  "Disconnect every connection (killing spawned images; attached ones keep
running), interrupt running jobs, and refuse later calls. Idempotent. → NIL"
  (let ((state (lisp-tools-state tools))
        (connections '())
        (jobs '()))
    (bt2:with-lock-held ((lm::state-lock state))
      (when (%closed tools) (return-from close-lisp-tools nil))
      (setf (%closed tools) t
            connections (loop for name being the hash-keys of (lm::state-connections state)
                                using (hash-value conn)
                              collect (cons name conn))
            jobs (remove-if-not #'lm::job-running-p (lm::state-jobs state))))
    ;; One dead wire must not keep the other images alive: a REPL-ERROR on
    ;; one connection is dropped and the rest are still closed.
    (dolist (job jobs)
      (when (lm::job-connection job)
        (handler-case (repl:repl-interrupt (lm::job-connection job)
                                           :thread (lm::job-interrupt-thread job))
          (repl:repl-error () nil))))
    (loop for (name . conn) in connections
          do (handler-case (repl:repl-disconnect conn :kill t) ; kills only spawned images
               (repl:repl-error () nil))
             (lm::remove-connection name :state state))
    nil))

(defun lisp-tools-instructions (tools)
  "Text for the agent's instructions: lisp-mcp's server instructions for the
source's groups, without the MCP resources line; :OBSERVE adds a line that
starts with \"OBSERVE MODE\"."
  (let* ((text (lm::server-instructions :state (lisp-tools-state tools)))
         (lines (remove-if (lambda (line) (alexandria:starts-with-subseq "Resources:" line))
                           (uiop:split-string text :separator '(#\Newline)))))
    (format nil "~{~A~^~%~}~@[~%~A~]" lines
            (and (eq (lisp-tools-mode tools) :observe)
                 "OBSERVE MODE: only read tools and repl-eval / debug-eval-in-frame / inspect code that scans to lisp:pure or lisp:reads run (repl-connect attaches; no spawn, edit, compile, load or restart). Anything else is refused as a tool result."))))

;;; ai-agent-protocol tool source

(defun %copy-schema (schema)
  "SCHEMA with fresh top-level and properties tables: TOOL-SCHEMA adds
arguments in place, and the registry's schema must stay as it is."
  (let ((copy (alexandria:copy-hash-table schema)))
    (alexandria:when-let ((props (gethash "properties" copy)))
      (setf (gethash "properties" copy) (alexandria:copy-hash-table props)))
    copy))

(defmethod agent:list-agent-tools ((source lisp-tools) &key context)
  "→ LLM-TOOL descriptors with lisp-mcp's names, descriptions and schemas (as
its server offers them for the source's groups), in catalog order."
  (declare (ignore context))
  (let ((lm:*state* (lisp-tools-state source)))
    (loop for name in (lisp-tools-names source)
          for entry = (%entry name)
          collect (llm:make-llm-tool :name name
                                     :description (second entry)
                                     :parameters (lm::tool-schema name (%copy-schema (third entry)))))))

(defmethod agent:tool-executable-p ((source lisp-tools) name &key context)
  (declare (ignore context))
  (%offered-p source name))

(defun %needs-approval-p (source name arguments)
  (let ((args (handler-case (%decode-arguments arguments)
                (agent:tool-input-error () nil)))
        (allow (lisp-tools-observe-allow source)))
    (flet ((unsafe-code-p ()
             (or (null args)
                 (not (observe-safe-resources-p (%code-resources source args) allow)))))
      (cond ((member name +code-tools+ :test #'string=)
             (if (and args (string= name "inspect") (%blank-p (lm::arg-string args "code")))
                 nil
                 (unsafe-code-p)))
            ((and (%prefix-p "edit-" name) args (lm::arg-bool args "compile"))
             (unsafe-code-p))
            ((member name +approval-tools+ :test #'string=) t)
            (t nil)))))

(defmethod agent:tool-needs-approval-p ((source lisp-tools) name arguments &key context)
  "With :NEEDS-APPROVAL true: code tools (and edits with compile=true) ask
unless their scan is observe-safe; +APPROVAL-TOOLS+ always ask; the rest never."
  (declare (ignore context))
  (and (lisp-tools-needs-approval source)
       (%needs-approval-p source name arguments)))

(defmethod agent:invoke-tool-async ((source lisp-tools) name arguments
                                    &key context callback error-callback)
  "CALL-LISP-TOOL off the event loop; the result string goes to CALLBACK, a
signalled condition to ERROR-CALLBACK."
  (if (or (%offered-p source name) (%observe-refused-tool-p source name))
      ;; The protocol's sync function-tool path: a worker thread, then the
      ;; value or the condition delivered on the loop.
      (agent:invoke-tool-async
       (agent:make-function-tool
        :name name
        :handler (lambda (args) (call-lisp-tool source name args)))
       name arguments
       :context context :callback callback :error-callback error-callback)
      (call-next-method)))

(defmethod agent:tool-permission-resources ((source lisp-tools) name arguments &key context)
  "Resources of the call (see the H4a brief): code scans, path:, system:,
restart:, image:, inspect:; (\"*\") when the arguments do not parse."
  (declare (ignore context))
  (%safe-resources source name arguments))
