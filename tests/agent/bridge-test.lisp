(in-package #:lisp-mcp/agent/tests)

;;; H4b: image events without an image (src/agent/events.lisp).

(defun fake-debugger (&key (thread 42) (level 1) (frames 12))
  "A DEBUGGER-STATE as the wire would parse it: two restarts, FRAMES frames."
  (make-instance 'repl:debugger-state
                 :thread thread :level level
                 :condition "bg boom"
                 :condition-type "[Condition of type SIMPLE-ERROR]"
                 :continuations '()
                 :restarts (list (make-instance 'repl:restart-info :index 0 :name "RETRY-LATER"
                                                                   :description "Retry later")
                                 (make-instance 'repl:restart-info :index 1 :name "*ABORT"
                                                                   :description "Return to top level"))
                 :frames (loop for i below frames
                               collect (make-instance 'repl:frame-info
                                                      :index i
                                                      :description (format nil "(FRAME-~D)" i)))))

(defun rendered (state)
  "lisp-mcp's terse debugger text of STATE with +IMAGE-EVENT-FRAMES+ frames."
  (let ((lisp-mcp::*verbose* nil)
        (lisp-mcp::*terse-frames* +image-event-frames+))
    (lisp-mcp::render-debugger state)))

(deftest bridge-defaults
  (ok (= 8 +image-event-frames+))
  (ok (equal "lisp-image-event" +image-event-tag+))
  (with-tools (tools root) '()
    (ok (eq t (lisp-tools-image-events-p tools)) "image events are on by default")
    (ok (null (lisp-tools-on-image-event tools)))
    (ok (null (lisp-tools-pending-events tools)) "nothing queued")
    (ok (null (take-image-events tools))))
  (let ((fn (lambda (tools event) (declare (ignore tools event)) nil)))
    (with-tools (tools root :image-events nil :on-image-event fn) '()
      (ok (null (lisp-tools-image-events-p tools)))
      (ok (eq fn (lisp-tools-on-image-event tools)))
      (setf (lisp-tools-on-image-event tools) nil)
      (ok (null (lisp-tools-on-image-event tools)) "the callback can be changed"))))

(deftest bridge-debugger-text
  (let* ((state (fake-debugger))
         (text (debugger-event-text "image" state)))
    (ok (stringp text))
    (ok (equal text
               (format nil "<lisp-image-event kind=\"debugger\" connection=\"image\" thread=\"42\" level=\"1\">~%image entered the debugger outside a tool call (thread 42, level 1).~%~AThe thread waits in the debugger until a restart is chosen: call debug-invoke-restart with connection \"image\" (debug-backtrace, debug-frame-locals and inspect-condition look first).~%</lisp-image-event>~%"
                       (rendered state)))
        "the exact debugger event text")
    (ok (has "(FRAME-7)" text) "eight frames are shown")
    (ok (not (has "(FRAME-8)" text)))
    (ok (has "[RETRY-LATER] Retry later" text))))

(deftest bridge-debugger-text-observe
  (let ((text (debugger-event-text "watch" (fake-debugger :thread 7 :level 2 :frames 3) :observe t)))
    (ok (has "<lisp-image-event kind=\"debugger\" connection=\"watch\" thread=\"7\" level=\"2\">" text))
    (ok (has "This source is in observe mode: it cannot choose a restart. Tell the user which restart you would choose and why." text))
    (ok (not (has "call debug-invoke-restart" text)) "no restart advice in observe mode")
    (ok (has "(FRAME-2)" text))
    (ok (alexandria:ends-with-subseq (format nil "</lisp-image-event>~%") text))))

(deftest bridge-event-turns
  (let* ((event (make-instance 'image-event :kind :debugger :connection "image" :thread 42 :level 1
                                            :text (debugger-event-text "image" (fake-debugger))))
         (turn (image-event-turn event)))
    (ok (image-event-p event))
    (ok (eq :user (llm:llm-turn-role turn)))
    (ok (equal (image-event-text event) (llm:turn-text turn)))
    (ok (image-event-turn-p turn))
    (ok (not (image-event-turn-p (llm:user-turn "please fix <lisp-image-event>"))) "the tag must lead")
    (ok (not (image-event-turn-p (llm:make-llm-turn :role :assistant
                                                    :parts (list (llm:make-llm-text-part
                                                                  :text (image-event-text event))))))
        "only user turns")))

(deftest bridge-instructions
  (with-tools (tools root) '()
    (let ((text (lisp-tools-instructions tools)))
      (ok (has "IMAGE EVENTS:" text))
      (ok (has "<lisp-image-event>" text))))
  (with-tools (tools root :mode :observe) '()
    (let* ((text (lisp-tools-instructions tools))
           (events (search "IMAGE EVENTS:" text))
           (observe (search "OBSERVE MODE" text)))
      (ok (and events observe (< events observe)) "before the OBSERVE MODE line")))
  (with-tools (tools root :image-events nil) '()
    (ok (not (has "IMAGE EVENTS:" (lisp-tools-instructions tools)))))
  (with-tools (tools root :groups "code") '()
    (ok (not (has "IMAGE EVENTS:" (lisp-tools-instructions tools))) "no image group, no events")))

(deftest bridge-closed-source
  (with-tools (tools root) '()
    (close-lisp-tools tools)
    (ok (null (lisp-tools-pending-events tools)))
    (ok (null (take-image-events tools)))))
