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
;;; Contract stubs: every function below that calls LISP-MCP::%NOT-IMPLEMENTED
;;; is the solver's. The class, its readers and the constants are given.

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

(defun make-lisp-tools (&key (root (uiop:getcwd)) groups tools (mode :full) observe-allow
                             (needs-approval t) (package "cl-user"))
  "A LISP-TOOLS rooted at ROOT (a directory that must exist). GROUPS selects
lisp-mcp tool groups like its --tools (NIL = all, a keyword list, or a comma
string with presets \"code\" / \"image\" / \"all\"). TOOLS, when given, keeps
only those names. MODE is :FULL or :OBSERVE. Unknown groups, tool names or
modes → ERROR."
  (declare (ignore root groups tools mode observe-allow needs-approval package))
  (lm::%not-implemented 'make-lisp-tools))

(defun observe-safe-resources-p (resources &optional allow)
  "True when every one of RESOURCES is \"lisp:pure\", matches \"lisp:reads *\",
or matches one of the globs ALLOW (ai-agent-protocol glob syntax)."
  (declare (ignore resources allow))
  (lm::%not-implemented 'observe-safe-resources-p))

(defun call-lisp-tool (tools name arguments)
  "Run tool NAME with ARGUMENTS (a JSON object string or a string-keyed hash
table) synchronously → its result string. A name not offered →
AGENT-UNKNOWN-TOOL; an observe refusal → OBSERVE-MODE-REFUSED; lisp-mcp's
argument / path / locator / edit errors → TOOL-INPUT-ERROR; no or unknown
connection, or any call after CLOSE-LISP-TOOLS → TOOL-UNAVAILABLE."
  (declare (ignore tools name arguments))
  (lm::%not-implemented 'call-lisp-tool))

(defun close-lisp-tools (tools)
  "Disconnect every connection (killing spawned images), cancel running jobs,
and refuse later calls. Idempotent. → NIL"
  (declare (ignore tools))
  (lm::%not-implemented 'close-lisp-tools))

(defun lisp-tools-instructions (tools)
  "Text for the agent's instructions: lisp-mcp's server instructions for the
source's groups, without the MCP resources line; :OBSERVE adds a line that
starts with \"OBSERVE MODE\"."
  (declare (ignore tools))
  (lm::%not-implemented 'lisp-tools-instructions))

;;; ai-agent-protocol tool source

(defmethod agent:list-agent-tools ((source lisp-tools) &key context)
  (declare (ignore context))
  (lm::%not-implemented 'agent:list-agent-tools))

(defmethod agent:tool-executable-p ((source lisp-tools) name &key context)
  (declare (ignore name context))
  (lm::%not-implemented 'agent:tool-executable-p))

(defmethod agent:tool-needs-approval-p ((source lisp-tools) name arguments &key context)
  (declare (ignore name arguments context))
  (lm::%not-implemented 'agent:tool-needs-approval-p))

(defmethod agent:invoke-tool-async ((source lisp-tools) name arguments
                                    &key context callback error-callback)
  (declare (ignore name arguments context callback error-callback))
  (lm::%not-implemented 'agent:invoke-tool-async))

(defmethod agent:tool-permission-resources ((source lisp-tools) name arguments &key context)
  (declare (ignore name arguments context))
  (lm::%not-implemented 'agent:tool-permission-resources))
