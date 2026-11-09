(in-package #:lisp-mcp/tests)

(deftest live-image-session
  (with-project (root)
    (write-project-file root "demo.lisp" (format nil "(defun half (x) (/ x 2))~%"))
    (ok (search "No live image" (call "repl-eval" "code" "1")))
    (ok (search "SBCL" (call "repl-spawn")))
    (unwind-protect
         (progn
           (ok (search "=> 3" (call "repl-eval" "code" "(+ 1 2)")))
           (ok (search "path:0 ok (0 notes)" (call "repl-compile" "path" "demo.lisp" "locator" "defun half")))
           (let ((debug (call "repl-eval" "code" "(half \"x\")")))
             (ok (search "DEBUGGER level 1" debug))
             (ok (search "ABORT" debug)))
           (ok (search "HALF" (call "debug-backtrace" "end" 10) :test #'char-equal))
           (ok (search "Aborted" (call "debug-invoke-restart" "restart" 0)))
           (ok (search "Not in the debugger" (call "debug-state")))
           (let ((r (call "repl-eval" "code" "(list 1 2)")))
             (ok (search "result id" r))
             (let ((id (parse-integer r :start (+ (search "result id " r) 10) :junk-allowed t)))
               (ok (search "CONS" (call "inspect" "result" id) :test #'char-equal))))
           (ok (search "LIST" (call "repl-arglist" "symbol" "mapcar") :test #'char-equal)))
      (call "repl-disconnect"))))

(defun wait-for (predicate &key (seconds 10))
  (loop repeat (* seconds 20)
        when (funcall predicate) return t
        do (sleep 0.05)))

(deftest live-notifications-resources-and-hybrid-tools
  (with-project (root)
    (write-project-file root "demo.lisp"
                        (format nil "(defun half (x) (/ x 2))~%~%(defun quarter (x) (half (half x)))~%~%(defun never-loaded () 0)~%"))
    (call "repl-spawn")
    (unwind-protect
         (collecting-notifications (sent)
           (call "repl-eval" "code" "(progn (defun half (x) (/ x 2)) (defun quarter (x) (half (half x))))")
           ;; live outline: which definitions the image has
           (let ((outline (call "source-outline" "path" "demo.lisp" "live" t)))
             (ok (search "half (x)  [path:0] [image: defined]" outline))
             (ok (search "never-loaded ()  [path:2] [image: not loaded]" outline)))
           (let ((refs (call "source-find-references" "symbol" "half" "live" t)))
             (ok (search "[image] who-calls" refs))
             (ok (search "QUARTER" refs :test #'char-equal)))
           ;; output the image sends outside a request (what a REPL's redirected
           ;; streams send) becomes a log message and a resource update
           (dispatch "resources/subscribe" "uri" "repl://swank-1/output")
           (call "repl-eval" "code" "(swank::send-to-emacs (list :write-string (format nil \"from-thread~%\")))")
           (ok (wait-for (lambda () (find "notifications/resources/updated" sent :key #'car :test #'string=)))
               "output resource update")
           (ok (wait-for (lambda () (search "from-thread" (resource-text "repl://swank-1/output"))))
               "repl://swank-1/output has the text")
           (ok (find-if (lambda (n) (and (string= (car n) "notifications/message")
                                         (search "from-thread" (gethash "data" (cdr n)))))
                        sent)
               "output log message")
           ;; entering the debugger is announced
           (call "repl-eval" "code" "(error \"announced\")")
           (ok (find-if (lambda (n) (and (string= (car n) "notifications/message")
                                         (equal "warning" (gethash "level" (cdr n)))
                                         (search "entered the debugger" (gethash "data" (cdr n)))))
                        sent))
           (ok (search "announced" (resource-text "repl://swank-1/debugger")))
           (call "debug-invoke-restart" "restart" 0)
           (ok (search "Not in the debugger" (resource-text "repl://swank-1/debugger")))
           ;; stepping
           (call "repl-eval" "code" "(defun stepped (x) (declare (optimize (debug 3) (speed 0))) (let ((y (1+ x))) (* y 2)))")
           (ok (search "DEBUGGER" (call "repl-eval" "code" "(step (stepped 1))")))
           (ok (loop repeat 20
                     for text = (call "debug-step" "mode" "out")
                     thereis (search "=> 4" text))))
      (call "repl-disconnect"))))

(deftest slynk-channels
  (with-project (root)
    (call "repl-spawn" "backend" "slynk")
    (unwind-protect
         (progn
           (ok (search "Channel channel-1 open" (call "repl-channel-open")))
           (let ((r (call "repl-eval" "channel" "channel-1" "code" "(list 1 2 3)")))
             (ok (search "=> (1 2 3)   #v0:0" r)))
           (ok (search "=> 3" (call "repl-eval" "channel" "channel-1" "code" "(length #v0:0)")))
           (ok (search "CONS" (call "inspect-history" "channel" "channel-1" "entry" 0) :test #'char-equal))
           (ok (search "channel-1: package" (call "repl-channel-list")))
           (ok (search "Closed channel-1" (call "repl-channel-close" "channel" "channel-1"))))
      (call "repl-disconnect"))
    ;; Swank has no channels
    (call "repl-spawn")
    (unwind-protect
         (multiple-value-bind (text error) (call "repl-channel-open")
           (ok error)
           (ok (search "backend slynk" text)))
      (call "repl-disconnect"))))

(deftest build-output-is-trimmed
  "repl-load-system / repl-run-tests returned every '; compiling file' and
   '; wrote' line plus Rove's colour codes; warnings and results stay."
  (let ((esc (code-char 27)))
    (ok (equal (lisp-mcp::quiet-build-output
                (format nil "; compiling file \"/a.lisp\" (written 1 NOV):~%~%; wrote /a.fasl~%~
                             ; compilation finished in 0:00:00.001~%WARNING: w~%~%~%~
                             ~C[32m✓ ~C[0m~C[90mExpect X~C[0m~%" esc esc esc esc))
               (format nil "WARNING: w~%~%✓ Expect X~%")))))
