;;;; Hidden grader for tasks/ledger-debug.md (two planted bugs: months-between
;;;; drops the last month, and an empty month divides by zero). The harness also checks that
;;;; tests/ledger-test.lisp is unchanged.
;;;;
;;;; The checks run in a worker thread with a deadline, so an agent's change
;;;; that loops cannot hang the grade.

(defun grade-exit (code fmt &rest args)
  (format t "~&~?~%" fmt args)
  (finish-output)
  (sb-ext:exit :code code :abort t))

(defun grade-fail (fmt &rest args)
  (apply #'grade-exit 1 (concatenate 'string "GRADE FAIL " fmt) args))

(handler-case (asdf:load-system "ledger")
  (error (c) (grade-fail "load: ~a" c)))

(defun call (name &rest args) (apply (find-symbol name "LEDGER") args))
(defun utc (y m d) (encode-universal-time 0 0 12 d m y 0))

(defun checks ()
  (handler-case
      (progn
        (let ((months (call "MONTHS-BETWEEN" "2025-11" "2026-02")))
          (unless (equal months '("2025-11" "2025-12" "2026-01" "2026-02"))
            (grade-fail "months-between across a year: ~s" months)))
        (let ((a (call "MAKE-ACCOUNT" "a")))
          (call "DEPOSIT" a 1000 :date (utc 2025 12 5))
          (unless (= 0 (call "AVERAGE-ENTRY" a "2026-03"))
            (grade-fail "average-entry of an empty month"))
          (unless (= 1000 (call "REPORT-TOTAL" (call "MONTHLY-REPORT" a "2025-10" "2026-03")))
            (grade-fail "report total")))
        (grade-exit 0 "GRADE PASS"))
    (error (c) (grade-fail "behaviour: ~a" c))))

(sb-thread:make-thread #'checks :name "grade")
(sleep 20)
(grade-fail "timed out after 20 s (a loop that never ends?)")
