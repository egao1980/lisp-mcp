(in-package #:lisp-mcp/agent/tests)

;;; LISP-TOOLS inside real agent runs (scripted LLM, libuv loop).

(defun lisp-agent (tools steps box &rest keys)
  (apply #'a:make-ai-agent :name "lisper" :backend (scripted-backend steps box)
                           :tools (list tools) keys))

(deftest agent-sees-the-source
  (with-tools (tools root :groups "image") '()
    (let ((agent (lisp-agent tools '("done") (list nil))))
      (ok (equal (lisp-tools-names tools) (mapcar #'llm:llm-tool-name (a:collect-run-tools agent))))
      (ok (eq tools (a:find-agent-tool agent "repl-eval")))
      (ok (eq tools (a:find-agent-tool agent "debug-invoke-restart"))))))

(deftest agent-fixes-a-seeded-bug-with-a-restart
  (with-agent-loop
    (with-tools (tools root) '()
      (let* ((run (a:run-ai-agent
                   (lisp-agent tools
                               (list (cons "repl-spawn" (json-args))
                                     (cons "repl-eval" (json-args "code" +seeded+))
                                     (cons "repl-eval" (json-args "code" "(seeded 1)"))
                                     (cons "debug-invoke-restart" (json-args "restart" 1))
                                     (cons "debug-invoke-restart" (json-args "restart" 0))
                                     (cons "repl-disconnect" (json-args))
                                     "done")
                               (list nil)
                               :permissions (list (a:make-permission-rule "*" "*" :allow)
                                                  (a:make-permission-rule
                                                   "debug-invoke-restart" "restart:abort" :deny
                                                   :feedback "choose the restart that returns a value")))
                   "fix it"))
             (results (tool-results run)))
        (ok (eq :stop (a:agent-run-finish-reason run)))
        (ok (= 6 (length results)))
        (ok (has "DEBUGGER level 1" (result-content (third results))))
        (ok (result-error-p (fourth results)) "the abort restart is denied")
        (ok (has "was not run" (result-content (fourth results))))
        (ok (has "choose the restart that returns a value" (result-content (fourth results))))
        (ok (not (result-error-p (fifth results))))
        (ok (has "=> 42" (result-content (fifth results))))))))

(deftest agent-pure-runs-mutating-asks
  (with-agent-loop
    (with-tools (tools root) '()
      (let* ((run (a:run-ai-agent
                   (lisp-agent tools
                               (list (cons "repl-eval" (json-args "code" "(+ 1 2)"))
                                     (cons "repl-eval" (json-args "code" "(setf *x* 1)"))
                                     "done")
                               (list nil))
                   "go"))
             (results (tool-results run)))
        (ok (eq :approval (a:agent-run-finish-reason run)) "the mutating eval pauses for approval")
        (ok (= 1 (length results)) "the pure eval ran without asking")
        (ok (has "No live image" (result-content (first results))))))))

(deftest agent-deny-rule-on-io
  (with-agent-loop
    (with-tools (tools root) '()
      (let* ((run (a:run-ai-agent
                   (lisp-agent tools
                               (list (cons "repl-eval" (json-args "code" "(print 1)"))
                                     "done")
                               (list nil)
                               :permissions (list (a:make-permission-rule "*" "*" :allow)
                                                  (a:make-permission-rule "repl-eval" "lisp:io" :deny
                                                                          :feedback "no output")))
                   "go"))
             (results (tool-results run)))
        (ok (eq :stop (a:agent-run-finish-reason run)))
        (ok (= 1 (length results)))
        (ok (has "was not run" (result-content (first results))))
        (ok (has "no output" (result-content (first results))))))))

(deftest agent-observe-refusal-is-a-tool-result
  (with-agent-loop
    (with-tools (tools root :mode :observe) '()
      (let* ((run (a:run-ai-agent
                   (lisp-agent tools
                               (list (cons "repl-eval" (json-args "code" "(setf *x* 1)"))
                                     "done")
                               (list nil)
                               :permissions (list (a:make-permission-rule "*" "*" :allow)))
                   "go"))
             (results (tool-results run)))
        (ok (eq :stop (a:agent-run-finish-reason run)) "a refusal does not abort the run")
        (ok (= 1 (length results)))
        (ok (result-error-p (first results)))
        (ok (has "observe" (result-content (first results))))))))
