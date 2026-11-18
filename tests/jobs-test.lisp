(in-package #:lisp-mcp/tests)

(deftest tool-descriptions-are-short
  (with-project (root)
    (let ((tools (mcp:list-tools *server*)))
      (ok (> (length tools) 50))
      (dolist (tool tools)
        (ok (<= (length (mcp:mcp-tool-description tool)) 120)
            (format nil "~A: ~D chars" (mcp:mcp-tool-name tool) (length (mcp:mcp-tool-description tool))))))
    (ok (< (length (mcp:mcp-server-instructions *server*)) 1700))))

(deftest job-arguments-key
  (let ((a (mcp:json-object "code" "(f)" "timeout" 5 "package" "X"))
        (b (mcp:json-object "package" "X" "code" "(f)" "verbose" t)))
    (ok (string= (lisp-mcp::job-arguments-key "repl-eval" a) (lisp-mcp::job-arguments-key "repl-eval" b)))
    (ok (string/= (lisp-mcp::job-arguments-key "repl-eval" a)
                  (lisp-mcp::job-arguments-key "repl-compile" a)))))

(deftest job-errors-reach-the-caller
  ;; the worker transfers the error: the call fails as a direct call would
  (with-project (root)
    (multiple-value-bind (text error) (call "repl-eval" "code" "1")
      (ok error)
      (ok (search "No live image" text)))
    (ok (search "No jobs" (call "job-list")))
    (multiple-value-bind (text error) (call "job-status" "id" "j99")
      (ok error)
      (ok (search "No job" text)))))

(defun job-id-in (text)
  (let ((p (search "as job " text)))
    (and p (subseq text (+ p 7) (position #\Space text :start (+ p 7))))))

(deftest long-calls-become-jobs
  (with-project (root)
    (call "repl-spawn")
    (unwind-protect
         (collecting-notifications (sent)
           (let* ((text (call "repl-eval" "code" "(progn (sleep 4) :slept)" "timeout" 1))
                  (id (job-id-in text)))
             (ok (search "Still running as job" text))
             (ok id)
             ;; the same call again does not start a second copy
             (ok (equal id (job-id-in (call "repl-eval" "code" "(progn (sleep 4) :slept)"))))
             ;; other work proceeds while the job runs
             (ok (search "=> 3" (call "repl-eval" "code" "(+ 1 2)")))
             (ok (search "running" (call "job-list")))
             (let ((done (call "job-wait" "id" id "timeout" 30)))
               (ok (search "done" done))
               (ok (search ":SLEPT" done)))
             (ok (search ":SLEPT" (call "job-status" "id" id)))
             (ok (find-if (lambda (n) (and (string= (car n) "notifications/message")
                                           (search (format nil "Job ~A finished" id) (gethash "data" (cdr n)))))
                          sent)
                 "finish log message"))
           ;; a fast call is not a job
           (call "repl-eval" "code" "(+ 2 2)")
           (ok (not (search "(+ 2 2)" (call "job-list"))))
           ;; cancel: the evaluation is interrupted and the job reports the debugger
           (let ((id (job-id-in (call "repl-eval" "code" "(sleep 60)" "timeout" 1))))
             (ok id)
             (ok (search "Interrupt sent" (call "job-cancel" "id" id)))
             (ok (search "DEBUGGER" (call "job-wait" "id" id "timeout" 30)))
             (call "debug-invoke-restart" "restart" 0)))
      (call "repl-disconnect"))))
