(in-package #:lisp-mcp/agent/tests)

;;; H4b: image events reaching an agent run's inbox (scripted LLM, libuv
;;; loop, a spawned Swank image).

(defun posting-callback (run-box)
  "ON-IMAGE-EVENT that posts every queued event into RUN-BOX's run once there is one."
  (lambda (tools event)
    (declare (ignore event))
    (let ((run (car run-box)))
      (when run (post-image-events tools run)))))

(defun capture-run (run-box)
  "An :ON-EVENT handler that keeps the run of the :STARTED event in RUN-BOX."
  (lambda (kind payload)
    (when (eq kind :started) (setf (car run-box) payload))))

(defun image-turn-position (turns)
  (position-if #'image-event-turn-p turns))

(defun tool-result-position (turns name-fragment)
  "Index of the first turn holding a tool result whose content has NAME-FRAGMENT."
  (position-if (lambda (turn)
                 (some (lambda (part)
                         (and (llm:llm-tool-result-part-p part)
                              (has name-fragment (llm:llm-tool-result-part-content part))))
                       (llm:llm-turn-parts turn)))
               turns))

(deftest agent-answers-a-background-debugger
  (with-agent-loop
    (let ((run-box (list nil)) (box (list nil)))
      (with-tools (tools root :on-image-event (posting-callback run-box)) '()
        (let* ((run (a:run-ai-agent
                     (lisp-agent tools
                                 (list (cons "repl-spawn" (json-args))
                                       (cons "repl-eval" (json-args "code" +background-error+))
                                       (cons "debug-invoke-restart" (json-args "restart" 1))
                                       (cons "debug-invoke-restart" (json-args "restart" 0))
                                       (cons "repl-disconnect" (json-args))
                                       "done")
                                 box
                                 :permissions (list (a:make-permission-rule "*" "*" :allow)
                                                    (a:make-permission-rule
                                                     "debug-invoke-restart" "restart:abort" :deny
                                                     :feedback "keep the thread: choose retry-later")))
                     "watch the worker"
                     :on-event (capture-run run-box)))
               (requests (car box))
               (results (tool-results run)))
          (ok (eq :stop (a:agent-run-finish-reason run)))
          (ok (= 6 (length requests)))
          (ok (null (image-turn-position (second requests))) "no event before the eval")
          (let* ((third-request (third requests))
                 (at (image-turn-position third-request)))
            (ok at "the event reached the run's inbox before the next generate")
            (ok (and at (> at (tool-result-position third-request ":STARTED")))
                "after the eval's result")
            (ok (and at (has "bg boom" (llm:turn-text (nth at third-request))))))
          (ok (= 1 (count-if #'image-event-turn-p (a:agent-run-turns run))) "posted once")
          (ok (result-error-p (third results)) "the abort restart is denied by the rule")
          (ok (has "keep the thread: choose retry-later" (result-content (third results))))
          (ok (not (result-error-p (fourth results))))
          (ok (has "Invoked restart 0 [RETRY-LATER]" (result-content (fourth results))))
          (ok (null (lisp-tools-pending-events tools)) "everything was taken"))))))

(deftest agent-unattended-restart-is-denied
  (with-agent-loop
    (let ((run-box (list nil)) (box (list nil)))
      (with-tools (tools root :on-image-event (posting-callback run-box)) '()
        (let* ((run (handler-bind ((a:agent-approval-required
                                     (lambda (c) (a:invoke-deny c "nobody can approve this run"))))
                      (a:run-ai-agent
                       (lisp-agent tools
                                   (list (cons "repl-spawn" (json-args))
                                         (cons "repl-eval" (json-args "code" +background-error+))
                                         (cons "debug-invoke-restart" (json-args "restart" 0))
                                         "done")
                                   box
                                   :permissions (list (a:make-permission-rule "repl-*" "*" :allow)))
                       "watch the worker"
                       :on-event (capture-run run-box))))
               (results (tool-results run)))
          (ok (eq :stop (a:agent-run-finish-reason run)))
          (ok (image-turn-position (third (car box))) "the event was delivered")
          (ok (result-error-p (third results)) "no rule allows the restart: the ask is denied")
          (ok (has "nobody can approve this run" (result-content (third results))))
          (ok (has "DEBUGGER level 1" (tool tools "debug-state"))
              "the thread still waits in the debugger"))))))

(deftest agent-idle-event-becomes-the-next-turn
  (with-agent-loop
    (let ((box (list nil)))
      (with-tools (tools root) '()
        (let ((run (a:run-ai-agent
                    (lisp-agent tools
                                (list (cons "repl-spawn" (json-args))
                                      (cons "repl-eval" (json-args "code" +late-background-error+))
                                      "started it"
                                      (cons "debug-invoke-restart" (json-args "restart" 0))
                                      "fixed")
                                box
                                :permissions (list (a:make-permission-rule "*" "*" :allow)))
                    "start the worker")))
          (ok (eq :stop (a:agent-run-finish-reason run)) "the turn ended before the error")
          (ok (= 3 (length (car box))))
          (ok (wait-until (lambda () (lisp-tools-pending-events tools)))
              "the event waits in the queue while no turn runs")
          (let ((events (take-image-events tools)))
            (ok (= 1 (length events)))
            (a:continue-ai-agent run (image-event-turn (first events))))
          (ok (eq :stop (a:agent-run-finish-reason run)))
          (ok (= 5 (length (car box))))
          (ok (image-event-turn-p (car (last (fourth (car box)))))
              "the follow-up turn is the event")
          (ok (has "late boom" (llm:turn-text (car (last (fourth (car box)))))))
          (ok (has "Invoked restart 0 [RETRY-LATER]"
                   (result-content (car (last (tool-results run)))))))))))
