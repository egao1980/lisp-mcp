(in-package #:lisp-mcp/agent/tests)

;;; H4b: image events from live images (repl-spawn: ros +Q run with Swank or
;;; Slynk). The asynchronous debugger entry is a background thread in the
;;; image; it waits in the debugger until a restart is chosen.

(defparameter +background-error+
  "(progn (sb-thread:make-thread (lambda () (restart-case (error \"bg boom\") (retry-later () :report \"Retry later\" 7))) :name \"bg-worker\") (sleep 1) :started)"
  "Starts a thread that enters the debugger at once, then returns :STARTED
after a second (the debugger event arrives while this call runs).")

(defparameter +late-background-error+
  "(progn (sb-thread:make-thread (lambda () (sleep 1.5) (restart-case (error \"late boom\") (retry-later () :report \"Retry later\" 7))) :name \"late-worker\") :started)"
  "Starts a thread that enters the debugger 1.5 s later; returns at once.")

(defun wait-until (predicate &key (timeout 20))
  "Poll PREDICATE every 50 ms until it returns true (→ its value) or TIMEOUT
seconds pass (→ NIL)."
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop
      (let ((value (funcall predicate)))
        (when value (return value)))
      (when (> (get-internal-real-time) deadline) (return nil))
      (sleep 0.05))))

(defun make-recorder ()
  "→ (values callback seen): CALLBACK records (tools . event) pairs; SEEN
returns them, oldest first."
  (let ((lock (bt2:make-lock :name "recorder")) (pairs '()))
    (values (lambda (tools event)
              (bt2:with-lock-held (lock) (push (cons tools event) pairs)))
            (lambda () (bt2:with-lock-held (lock) (reverse pairs))))))

(defun seen-kind (seen kind)
  (remove-if-not (lambda (pair) (eq kind (image-event-kind (cdr pair)))) (funcall seen)))

(deftest-parametrize live-background-debugger-is-an-event
    ((backend) :ids ("swank" "slynk") ("swank") ("slynk"))
  (multiple-value-bind (callback seen) (make-recorder)
    (with-tools (tools root :on-image-event callback) '()
      (let ((name (format nil "~A-1" backend)))
        (ok (has "SBCL" (tool tools "repl-spawn" "backend" backend)))
        (ok (has "=> :STARTED" (tool tools "repl-eval" "code" +background-error+))
            "the eval itself returns: the debugger is in another thread")
        (ok (wait-until (lambda () (lisp-tools-pending-events tools))) "the event is queued")
        (let ((event (first (lisp-tools-pending-events tools))))
          (ok (image-event-p event))
          (ok (eq :debugger (image-event-kind event)))
          (ok (equal name (image-event-connection event)))
          (ok (image-event-thread event))
          (ok (eql 1 (image-event-level event)))
          (ok (has "bg boom" (image-event-condition event)))
          (ok (equal '(0 "retry-later" "Retry later") (first (image-event-restarts event))))
          (ok (find "abort" (image-event-restarts event) :key #'second :test #'string=))
          (ok (<= 1 (length (image-event-frames event)) +image-event-frames+))
          (ok (every (lambda (f) (and (integerp (first f)) (stringp (second f))))
                     (image-event-frames event)))
          (ok (null (image-event-job event)))
          (ok (alexandria:starts-with-subseq
               (format nil "<lisp-image-event kind=\"debugger\" connection=\"~A\" thread=\"~A\" level=\"1\">"
                       name (image-event-thread event))
               (image-event-text event)))
          (ok (has "bg boom" (image-event-text event)))
          (ok (has "[RETRY-LATER] Retry later" (image-event-text event)))
          (ok (has "call debug-invoke-restart" (image-event-text event)))
          (ok (equal (list (cons tools event)) (seen-kind seen :debugger))
              "ON-IMAGE-EVENT got it once, with the source")
          (sleep 0.5)
          (ok (= 1 (length (lisp-tools-pending-events tools))) "one event, not two")
          (ok (equal '("restart:retry-later") (resources tools "debug-invoke-restart" "restart" 0))
              "the restart gate sees the background debugger")
          (ok (approval-p tools "debug-invoke-restart" "restart" 0) "a restart still asks")
          (ok (equal (list event) (take-image-events tools)))
          (ok (null (lisp-tools-pending-events tools)) "taken")
          (let ((r (tool tools "debug-invoke-restart" "restart" 0)))
            (ok (has "Invoked restart 0 [RETRY-LATER] in thread" r))
            (ok (has "no tool call was waiting on that thread" r)))
          (ok (wait-until (lambda () (seen-kind seen :debugger-return))) "the image left the debugger")
          (let ((back (cdr (first (seen-kind seen :debugger-return)))))
            (ok (equal name (image-event-connection back)))
            (ok (equal (image-event-thread event) (image-event-thread back)))
            (ok (eql 1 (image-event-level back))))
          (ok (null (lisp-tools-pending-events tools)) "a debugger return is never queued")
          (ok (has "Not in the debugger" (tool tools "debug-state"))))))))

(deftest live-debugger-return-drops-a-stale-event
  (with-tools (tools root) '()
    (tool tools "repl-spawn")
    (tool tools "repl-eval" "code" +background-error+)
    (ok (wait-until (lambda () (lisp-tools-pending-events tools))))
    ;; Left without taking the event: nothing waits in that debugger any more.
    (ok (has "Invoked restart 1 [" (tool tools "debug-invoke-restart" "restart" 1)))
    (ok (wait-until (lambda () (null (lisp-tools-pending-events tools))))
        "the queued event is dropped")))

(deftest-parametrize live-tool-call-debugger-is-the-result
    ((backend) :ids ("swank" "slynk") ("swank") ("slynk"))
  (multiple-value-bind (callback seen) (make-recorder)
    (with-tools (tools root :on-image-event callback) '()
      (tool tools "repl-spawn" "backend" backend)
      (tool tools "repl-eval" "code" +seeded+)
      (let ((debug (tool tools "repl-eval" "code" "(seeded 1)")))
        (ok (has "DEBUGGER level 1" debug) "the call returns the debugger state, it does not hang")
        (ok (has "RETURN-DEFAULT" debug)))
      (sleep 1)
      (ok (null (lisp-tools-pending-events tools)) "the call's own debugger is not an event")
      (ok (null (seen-kind seen :debugger)))
      (ok (equal '("restart:return-default") (resources tools "debug-invoke-restart" "restart" 0)))
      (ok (approval-p tools "debug-invoke-restart" "restart" 0))
      (ok (has "=> 42" (tool tools "debug-invoke-restart" "restart" 0))
          "the waiting call's values come back (H4a)")
      (ok (null (lisp-tools-pending-events tools))))))

(defun running-job-id (text)
  "The jN of a \"Still running as job jN\" result, or NIL."
  (let ((at (search "Still running as job " text)))
    (and at (let* ((start (+ at (length "Still running as job ")))
                   (end (position #\Space text :start start)))
              (subseq text start end)))))

(deftest live-detached-jobs-are-events
  ;; Every job tool call starts a job (and takes an id), so the ids are read
  ;; from the results.
  (multiple-value-bind (callback seen) (make-recorder)
    (with-tools (tools root :on-image-event callback) '()
      (tool tools "repl-spawn")
      (tool tools "repl-eval" "code" +seeded+)
      (ok (has "=> 3" (tool tools "repl-eval" "code" "(+ 1 2)")))
      (let ((id (running-job-id (tool tools "repl-eval" "code" "(progn (sleep 2) :done)" "timeout" 1))))
        (ok id "the call detached")
        (ok (wait-until (lambda () (lisp-tools-pending-events tools))))
        (let ((event (first (take-image-events tools))))
          (ok (eq :job (image-event-kind event)))
          (ok (equal id (image-event-job event)))
          (ok (eq :done (image-event-job-state event)))
          (ok (equal "swank-1" (image-event-connection event)))
          (ok (alexandria:starts-with-subseq
               (format nil "<lisp-image-event kind=\"job\" connection=\"swank-1\" job=\"~A\" state=\"done\">" id)
               (image-event-text event)))
          (ok (has (format nil "Job ~A (" id) (image-event-text event)))
          (ok (has ":DONE" (image-event-text event)))))
      (let ((id (running-job-id (tool tools "repl-eval" "code" "(progn (sleep 2) (seeded 1))" "timeout" 1))))
        (ok id)
        (ok (wait-until (lambda () (lisp-tools-pending-events tools))))
        (sleep 0.5)
        (let ((events (take-image-events tools)))
          (ok (= 1 (length events)) "the job's debugger is reported once, by the job")
          (ok (eq :job (image-event-kind (first events))))
          (ok (equal id (image-event-job (first events))))
          (ok (has "DEBUGGER level 1" (image-event-text (first events))))))
      (ok (null (seen-kind seen :debugger)))
      (ok (has "=> 42" (tool tools "debug-invoke-restart" "restart" 0))
          "the job's request gets its value")
      (ok (has "=> 3" (tool tools "repl-eval" "code" "(+ 1 2)")))
      (sleep 0.5)
      (ok (null (lisp-tools-pending-events tools)) "a call that finishes in time is no event"))))

(deftest live-image-events-off
  (multiple-value-bind (callback seen) (make-recorder)
    (with-tools (tools root :image-events nil :on-image-event callback) '()
      (tool tools "repl-spawn")
      (tool tools "repl-eval" "code" +background-error+)
      (sleep 2)
      (ok (null (lisp-tools-pending-events tools)))
      (ok (null (funcall seen)) "the callback is never called")
      (ok (has "DEBUGGER level 1" (tool tools "debug-state")) "debug-state still sees it (H4a)")
      (tool tools "debug-invoke-restart" "restart" 1))))

(deftest live-close-drops-events
  (multiple-value-bind (callback seen) (make-recorder)
    (with-tools (tools root :on-image-event callback) '()
      (tool tools "repl-spawn")
      (tool tools "repl-eval" "code" +background-error+)
      (ok (wait-until (lambda () (lisp-tools-pending-events tools))))
      (close-lisp-tools tools)
      (ok (null (lisp-tools-pending-events tools)))
      (ok (null (take-image-events tools)))
      (ok (= 1 (length (seen-kind seen :debugger)))))))
