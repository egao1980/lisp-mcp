;;;; Hidden grader for tasks/local-time-quarter.md.

(defun grade-fail (fmt &rest args)
  (format t "~&GRADE FAIL ~?~%" fmt args)
  (uiop:quit 1))

(handler-case (asdf:load-system "local-time")
  (error (c) (grade-fail "load: ~a" c)))

(defun lt (name)
  (or (find-symbol name "LOCAL-TIME") (grade-fail "missing ~a" name)))

(multiple-value-bind (s status) (find-symbol "TIMESTAMP-QUARTER" "LOCAL-TIME")
  (unless (and s (eq status :external)) (grade-fail "timestamp-quarter is not exported")))

(let* ((utc (symbol-value (lt "+UTC-ZONE+")))
       (plus1 (funcall (lt "%MAKE-SIMPLE-TIMEZONE") "Plus One" "P1" 3600))
       (quarter (lt "TIMESTAMP-QUARTER"))
       (fmt (lt "FORMAT-TIMESTRING")))
  (flet ((ts (y mo d h mi)
           (funcall (lt "ENCODE-TIMESTAMP") 0 0 mi h d mo y :timezone utc)))
    (handler-case
        (progn
          (loop for (month want) in '((1 1) (2 1) (3 1) (4 2) (6 2) (7 3) (9 3) (10 4) (12 4))
                for got = (funcall quarter (ts 2026 month 10 12 0) :timezone utc)
                unless (eql got want)
                  do (grade-fail "quarter of month ~a is ~a, want ~a" month got want))
          (let ((edge (ts 2026 3 31 23 30)))
            (unless (eql 1 (funcall quarter edge :timezone utc))
              (grade-fail "2026-03-31T23:30Z in UTC"))
            (unless (eql 2 (funcall quarter edge :timezone plus1))
              (grade-fail "2026-03-31T23:30Z in +01:00 is April: quarter 2")))
          (let ((s (funcall fmt nil (ts 2026 2 10 12 0) :format '(:year "-Q" :quarter) :timezone utc)))
            (unless (equal s "2026-Q1") (grade-fail "format :quarter gave ~s" s)))
          (let ((s (funcall fmt nil (ts 2026 11 10 12 0) :format '((:quarter 2)) :timezone utc)))
            (unless (equal s "04") (grade-fail "format (:quarter 2) gave ~s" s)))
          ;; existing formats untouched
          (let ((s (funcall fmt nil (ts 2026 2 10 12 0)
                            :format (symbol-value (lt "+ISO-8601-DATE-FORMAT+")) :timezone utc)))
            (unless (equal s "2026-02-10") (grade-fail "iso date format broke: ~s" s))))
      (error (c) (grade-fail "behaviour: ~a" c)))))

(format t "~&GRADE PASS~%")
(uiop:quit 0)
