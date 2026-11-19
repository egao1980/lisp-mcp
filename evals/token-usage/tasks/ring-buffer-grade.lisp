;;;; Hidden grader for tasks/ring-buffer.md. Run in the agent's directory:
;;;;   cl-repo run -- ros -l ring-buffer-grade.lisp
;;;; Prints GRADE PASS or GRADE FAIL <reason> and exits 0 / 1.

(defun grade-fail (fmt &rest args)
  (format t "~&GRADE FAIL ~?~%" fmt args)
  (uiop:quit 1))

(handler-case (asdf:load-system "ring-buffer")
  (error (c) (grade-fail "load: ~a" c)))

(defun sym (name)
  (or (find-symbol name "RING-BUFFER") (grade-fail "missing export ~a" name)))

(defun call (name &rest args) (apply (sym name) args))

(handler-case
    (let ((r (call "MAKE-RING" 2)))
      (unless (zerop (call "RING-COUNT" r)) (grade-fail "new ring not empty"))
      (call "RING-PUSH" r 1)
      (call "RING-PUSH" r 2)
      (unless (call "RING-FULL-P" r) (grade-fail "ring of 2 with 2 items not full"))
      (call "RING-PUSH" r 3)
      (unless (equal '(2 3) (call "RING->LIST" r))
        (grade-fail "overwrite: ring->list is ~s, want (2 3)" (call "RING->LIST" r)))
      (unless (eql 2 (call "RING-POP" r)) (grade-fail "pop is not the oldest"))
      (unless (eql 1 (call "RING-COUNT" r)) (grade-fail "count after pop"))
      (call "RING-POP" r)
      (let ((empty (sym "RING-EMPTY")))
        (unless (subtypep empty 'error) (grade-fail "ring-empty is not an error"))
        (unless (eq :fallback
                    (handler-bind ((error (lambda (c)
                                            (when (typep c empty)
                                              (invoke-restart 'use-value :fallback)))))
                      (call "RING-POP" r)))
          (grade-fail "use-value restart on an empty ring"))))
  (error (c) (grade-fail "behaviour: ~a" c)))

(format t "~&GRADE PASS~%")
(uiop:quit 0)
