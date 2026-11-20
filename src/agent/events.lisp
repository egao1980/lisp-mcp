(in-package #:lisp-mcp/agent)

;;; H4b: image events, the debugger → agent inbox bridge.
;;;
;;; A LISP-TOOLS made with :IMAGE-EVENTS T (the default) watches every
;;; connection it adds (repl-spawn, repl-connect) and the jobs it starts:
;;;
;;; Which entries are events
;;;   :debugger      The image entered the debugger in a thread where no
;;;                  request is pending: the wire's (:debug …) carries
;;;                  DEBUGGER-STATE-CONTINUATIONS NIL (a background thread,
;;;                  a timer, any thread no tool call started), or :UNKNOWN
;;;                  (the server did not say). A debugger entry whose
;;;                  continuations name a pending request belongs to that
;;;                  request: a synchronous tool call returns it as its
;;;                  result (H4a: "DEBUGGER level N …"), and a detached job
;;;                  reports it as a :JOB event. So nothing is reported twice.
;;;   :job           A job the caller was told is "Still running as job jN"
;;;                  (detached) finished, failed, or ended in the debugger.
;;;                  A job that finishes within its call's wait is that call's
;;;                  result, not an event.
;;;   :debugger-return  The image left a debugger level (:debug-return).
;;;                  Not queued: only passed to ON-IMAGE-EVENT. It also drops
;;;                  the queued :DEBUGGER events of that connection and thread
;;;                  at that level or deeper (they are stale: nothing waits
;;;                  there any more). Events already taken stay taken.
;;;
;;; The queue
;;;   :DEBUGGER and :JOB events are queued, oldest first, under the state's
;;;   lock. A :DEBUGGER event for a connection, thread and level that is
;;;   already queued replaces it (at the end). LISP-TOOLS-PENDING-EVENTS
;;;   copies the queue; TAKE-IMAGE-EVENTS empties it; POST-IMAGE-EVENTS takes
;;;   it and posts each event's turn into an agent run with
;;;   AI-AGENT-PROTOCOL:AGENT-RUN-POST, which the run injects before its next
;;;   generate. CLOSE-LISP-TOOLS empties the queue and no event is made after
;;;   it. With :IMAGE-EVENTS NIL nothing is queued and ON-IMAGE-EVENT is never
;;;   called (the H4a behaviour: the debugger state is only seen through
;;;   debug-state).
;;;
;;; ON-IMAGE-EVENT
;;;   Called with (TOOLS EVENT) for every event, on the thread that produced
;;;   it (the wire's reader thread, or a job's worker), after a queued event
;;;   is queued. It must not block and must not call this source's image
;;;   tools (the reader thread would wait for itself). An error in it becomes
;;;   a WARNING; the event stays queued.
;;;
;;; Slots of an event (readers below)
;;;   kind        :debugger :job :debugger-return
;;;   connection  the connection name ("swank-1", "image", …)
;;;   thread      the image thread id as the wire gives it (:debugger,
;;;               :debugger-return), else NIL
;;;   level       the debugger level (:debugger, :debugger-return), else NIL
;;;   condition   the printed condition (:debugger), else NIL
;;;   restarts    (:debugger) ((index name description) …) in order, NAME in
;;;               the restart: resource form of H4a (downcased, no package
;;;               prefix, no leading *: "retry-later", "abort"), so a product
;;;               can show the rule that would allow it
;;;   frames      (:debugger) ((index description) …), the top
;;;               +IMAGE-EVENT-FRAMES+ at most
;;;   job         (:job) the job id "jN", else NIL
;;;   job-state   (:job) :done or :failed, else NIL
;;;   text        the message the model gets (below); "" for :debugger-return
;;;
;;; The text (one user turn; the tag is +IMAGE-EVENT-TAG+)
;;;   :debugger
;;;     <lisp-image-event kind="debugger" connection="C" thread="T" level="L">
;;;     C entered the debugger outside a tool call (thread T, level L).
;;;     <LISP-MCP::RENDER-DEBUGGER of the state, not verbose, with
;;;      LISP-MCP::*TERSE-FRAMES* bound to +IMAGE-EVENT-FRAMES+>
;;;     The thread waits in the debugger until a restart is chosen: call debug-invoke-restart with connection "C" (debug-backtrace, debug-frame-locals and inspect-condition look first).
;;;     </lisp-image-event>
;;;   In :OBSERVE mode the last line before the closing tag is instead:
;;;     This source is in observe mode: it cannot choose a restart. Tell the user which restart you would choose and why.
;;;   :job
;;;     <lisp-image-event kind="job" connection="C" job="jN" state="done|failed">
;;;     Job jN (<LISP-MCP::JOB-SUMMARY>) finished|failed after S s:
;;;     <the job's result text, trailing newlines trimmed>
;;;     </lisp-image-event>
;;;   (connection="C" is left out when the job has no connection.) Every line
;;;   ends with LF, the closing tag too.
;;;
;;; Tool results (lisp-mcp core, MCP and native alike)
;;;   debug-invoke-restart on a debugger level that no request waits for
;;;   (DEBUGGER-STATE-REQUEST-ID NIL, as for every :DEBUGGER event) returns
;;;     Invoked restart N [NAME] in thread T; no tool call was waiting on that thread, so its values are not shown.
;;;   (NAME as the wire prints it, e.g. RETRY-LATER), instead of "; No values".
;;;   Restarts still go through the H4a gate: resources ("restart:<name>"),
;;;   +APPROVAL-TOOLS+ ask, and :OBSERVE refuses the tool.
;;;
;;; Instructions
;;;   With :IMAGE-EVENTS T and an image group offered, LISP-TOOLS-INSTRUCTIONS
;;;   gets one more line, after lisp-mcp's lines and before the OBSERVE MODE
;;;   line, that starts with "IMAGE EVENTS:" and names <lisp-image-event>.
;;;
;;; Wiring (left to the implementation): a per-state hook in lisp-mcp core
;;; that CONNECTION-LISTENER calls for every connection event, and one that
;;; FINISH-JOB calls for a detached job, both set by MAKE-LISP-TOOLS. The MCP
;;; server's notifications stay as they are.

(defparameter +image-event-frames+ 8
  "Frames an image event shows (its FRAMES slot and its text).")

(defparameter +image-event-tag+ "lisp-image-event"
  "The XML-ish tag around an image event's text.")

(defclass image-event ()
  ((kind :initarg :kind :reader image-event-kind
         :type (member :debugger :job :debugger-return))
   (connection :initarg :connection :initform nil :reader image-event-connection)
   (thread :initarg :thread :initform nil :reader image-event-thread)
   (level :initarg :level :initform nil :reader image-event-level)
   (condition :initarg :condition :initform nil :reader image-event-condition)
   (restarts :initarg :restarts :initform '() :reader image-event-restarts)
   (frames :initarg :frames :initform '() :reader image-event-frames)
   (job :initarg :job :initform nil :reader image-event-job)
   (job-state :initarg :job-state :initform nil :reader image-event-job-state)
   (text :initarg :text :initform "" :reader image-event-text))
  (:documentation "Something a Lisp image did outside a tool call (see the file header)."))

(defun image-event-p (x) (typep x 'image-event))

(defmethod print-object ((event image-event) stream)
  (print-unreadable-object (event stream :type t)
    (format stream "~(~A~) ~A~@[ thread ~A~]~@[ level ~A~]~@[ ~A~]"
            (image-event-kind event) (image-event-connection event)
            (image-event-thread event) (image-event-level event) (image-event-job event))))

(defparameter +observe-footer+
  "This source is in observe mode: it cannot choose a restart. Tell the user which restart you would choose and why."
  "The last line of a :DEBUGGER event text in :OBSERVE mode.")

(defun debugger-event-text (connection state &key observe)
  "The :DEBUGGER event text (see the file header) for STATE (a
REPL-PROTOCOL:DEBUGGER-STATE) on CONNECTION (a name). OBSERVE: the observe
mode footer instead of the restart one. Pure: no wire call."
  (let ((thread (repl:debugger-state-thread state))
        (level (repl:debugger-state-level state))
        (rendered (let ((lm::*verbose* nil)
                        (lm::*terse-frames* +image-event-frames+))
                    (lm::render-debugger state)))) ; ends in LF
    (format nil "<~A kind=\"debugger\" connection=\"~A\" thread=\"~A\" level=\"~A\">~%~A entered the debugger outside a tool call (thread ~A, level ~A).~%~A~A~%</~A>~%"
            +image-event-tag+ connection thread level
            connection thread level
            rendered
            (if observe
                +observe-footer+
                (format nil "The thread waits in the debugger until a restart is chosen: call debug-invoke-restart with connection \"~A\" (debug-backtrace, debug-frame-locals and inspect-condition look first)."
                        connection))
            +image-event-tag+)))

(defun %job-event-text (connection job)
  "The :JOB event text (see the file header) for finished JOB on CONNECTION
(a name, or NIL)."
  (let ((id (lm::job-id job))
        (job-state (lm::job-state job)))
    (format nil "<~A kind=\"job\"~@[ connection=\"~A\"~] job=\"~A\" state=\"~(~A~)\">~%Job ~A (~A) ~:[finished~;failed~] after ~D s:~%~A~%</~A>~%"
            +image-event-tag+ connection id job-state
            id (lm::job-summary job) (eq job-state :failed) (lm::job-seconds job)
            (string-right-trim '(#\Newline) (or (lm::job-result job) ""))
            +image-event-tag+)))

;;; Making events. The hooks run on the producing thread (the wire's reader,
;;; or a job's worker). Every read or write of the queue, and the CLOSED check
;;; that decides whether an event is made, is under the state's lock;
;;; ON-IMAGE-EVENT runs after the lock is released.

(defun %lock (tools) (lm::state-lock (lisp-tools-state tools)))

(defun %unowned-p (state)
  "True when no pending request owns debugger STATE: its continuations are NIL
or :UNKNOWN."
  (let ((continuations (repl:debugger-state-continuations state)))
    (or (null continuations) (eq continuations :unknown))))

(defun %debugger-event (tools connection state)
  (make-instance 'image-event
                 :kind :debugger :connection connection
                 :thread (repl:debugger-state-thread state)
                 :level (repl:debugger-state-level state)
                 :condition (repl:debugger-state-condition state)
                 :restarts (mapcar (lambda (r)
                                     (list (repl:restart-info-index r)
                                           (%restart-name (repl:restart-info-name r))
                                           (repl:restart-info-description r)))
                                   (repl:debugger-state-restarts state))
                 :frames (loop for f in (repl:debugger-state-frames state)
                               repeat +image-event-frames+
                               collect (list (repl:frame-info-index f)
                                             (repl:frame-info-description f)))
                 :text (debugger-event-text connection state
                                            :observe (eq (lisp-tools-mode tools) :observe))))

(defun %same-level-p (a b)
  "True when queued event A is a :DEBUGGER event for B's connection, thread and level."
  (and (eq (image-event-kind a) :debugger)
       (equal (image-event-connection a) (image-event-connection b))
       (equal (image-event-thread a) (image-event-thread b))
       (eql (image-event-level a) (image-event-level b))))

(defun %enqueue (tools event)
  "Queue EVENT at the end (a :DEBUGGER event replaces its connection, thread
and level twin), unless TOOLS is closed. → true when queued."
  (bt2:with-lock-held ((%lock tools))
    (unless (%closed tools)
      (setf (%events tools)
            (append (if (eq (image-event-kind event) :debugger)
                        (remove-if (lambda (e) (%same-level-p e event)) (%events tools))
                        (%events tools))
                    (list event)))
      t)))

(defun %deliver (tools event)
  "Call ON-IMAGE-EVENT with (TOOLS EVENT), outside every lock. Its errors
become warnings."
  (alexandria:when-let ((fn (lisp-tools-on-image-event tools)))
    (handler-case (funcall fn tools event)
      (error (e) (warn "lisp-tools ON-IMAGE-EVENT failed on ~A: ~A" event e)))))

(defun %produce (tools event)
  (when (%enqueue tools event)
    (%deliver tools event)))

(defun %debugger-returned (tools connection thread level)
  "CONNECTION's THREAD left debugger LEVEL: drop the queued :DEBUGGER events
of that thread at LEVEL or deeper, then pass a :DEBUGGER-RETURN event to
ON-IMAGE-EVENT (it is not queued)."
  (when (bt2:with-lock-held ((%lock tools))
          (unless (%closed tools)
            (setf (%events tools)
                  (remove-if (lambda (e)
                               (and (eq (image-event-kind e) :debugger)
                                    (equal (image-event-connection e) connection)
                                    (equal (image-event-thread e) thread)
                                    (realp (image-event-level e)) (realp level)
                                    (>= (image-event-level e) level)))
                             (%events tools)))
            t))
    (%deliver tools (make-instance 'image-event :kind :debugger-return :connection connection
                                                :thread thread :level level))))

(defun %connection-event (tools name kind data)
  "The connection hook: event KIND with DATA from connection NAME."
  (case kind
    (:debug
     ;; a level a pending request owns is that request's result (or its job's event)
     (when (%unowned-p data)
       (%produce tools (%debugger-event tools name data))))
    (:debug-return
     (destructuring-bind (thread level &rest more) data
       (declare (ignore more))
       (%debugger-returned tools name thread level)))))

(defun %connection-name (state conn)
  "The name CONN is registered under in STATE, or NIL. Takes the state lock."
  (and conn
       (bt2:with-lock-held ((lm::state-lock state))
         (loop for name being the hash-keys of (lm::state-connections state)
                 using (hash-value c)
               when (eq c conn) return name))))

(defun %job-event (tools job)
  "The job hook: detached JOB has ended."
  (let ((name (%connection-name (lisp-tools-state tools) (lm::job-connection job))))
    (%produce tools (make-instance 'image-event
                                   :kind :job :connection name
                                   :job (lm::job-id job) :job-state (lm::job-state job)
                                   :text (%job-event-text name job)))))

(defun %install-image-hooks (tools)
  "Set the hooks of TOOLS' state (MAKE-LISP-TOOLS, with :IMAGE-EVENTS T);
CLOSE-LISP-TOOLS clears them."
  (let ((state (lisp-tools-state tools)))
    (bt2:with-lock-held ((lm::state-lock state))
      (setf (lm::state-connection-hook state)
            (lambda (name kind data) (%connection-event tools name kind data))
            (lm::state-job-hook state)
            (lambda (job) (%job-event tools job))))
    tools))

;;; The queue and turns

(defun lisp-tools-pending-events (tools)
  "A copy of TOOLS' queued image events, oldest first."
  (bt2:with-lock-held ((%lock tools))
    (copy-list (%events tools))))

(defun take-image-events (tools)
  "TOOLS' queued image events, oldest first; the queue is left empty."
  (bt2:with-lock-held ((%lock tools))
    (shiftf (%events tools) '())))

(defun image-event-turn (event)
  "The LLM user turn for EVENT: its text."
  (llm:user-turn (image-event-text event)))

(defun image-event-turn-p (turn)
  "True when TURN is a user turn whose text starts with \"<lisp-image-event\"."
  (and (llm:llm-turn-p turn)
       (eq :user (llm:llm-turn-role turn))
       (let ((text (llm:turn-text turn)))
         (and (stringp text)
              (alexandria:starts-with-subseq (concatenate 'string "<" +image-event-tag+) text)))
       t))

(defun post-image-events (tools run)
  "Take TOOLS' queued events and AGENT-RUN-POST each one's turn into RUN
(an AI-AGENT-PROTOCOL:AGENT-RUN), oldest first. → the events posted. Safe
from any thread."
  (let ((events (take-image-events tools))) ; the lock is released here
    (dolist (event events events)
      (agent:agent-run-post run (image-event-turn event)))))
