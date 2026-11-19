;;;; Hidden grader for tasks/ledger-edit.md. Prints GRADE PASS or GRADE FAIL <reason>.

(defun grade-fail (fmt &rest args)
  (format t "~&GRADE FAIL ~?~%" fmt args)
  (uiop:quit 1))

(handler-case (asdf:load-system "ledger")
  (error (c) (grade-fail "load: ~a" c)))

(defun ext (name)
  (multiple-value-bind (s status) (find-symbol name "LEDGER")
    (unless (eq status :external) (grade-fail "~a is not exported" name))
    s))

(defun call (name &rest args) (apply (ext name) args))

(defun utc (y m d) (encode-universal-time 0 0 12 d m y 0))

(handler-case
    (progn
      ;; 1. rename
      (multiple-value-bind (old status) (find-symbol "ACCT-BAL" "LEDGER")
        (when (or (eq status :external) (and old (fboundp old)))
          (grade-fail "acct-bal still exists")))
      (let ((a (call "MAKE-ACCOUNT" "a")))
        (call "DEPOSIT" a 500)
        (unless (= 500 (call "ACCOUNT-BALANCE" a)) (grade-fail "account-balance")))
      ;; 2. currency-mismatch + convert
      (let* ((mismatch (ext "CURRENCY-MISMATCH"))
             (eur (call "MAKE-ACCOUNT" "eur" :currency "EUR"))
             (usd (call "MAKE-ACCOUNT" "usd" :currency "USD")))
        (unless (subtypep mismatch (ext "LEDGER-ERROR"))
          (grade-fail "currency-mismatch is not a ledger-error"))
        (call "DEPOSIT" eur 10000)
        (let ((c (handler-case (progn (call "TRANSFER" eur usd 1000) nil)
                   (error (c) c))))
          (unless (typep c mismatch) (grade-fail "no currency-mismatch, got ~s" c))
          (unless (and (eq eur (call "CURRENCY-MISMATCH-FROM" c))
                       (eq usd (call "CURRENCY-MISMATCH-TO" c)))
            (grade-fail "currency-mismatch readers")))
        (unless (and (= 10000 (call "ACCOUNT-BALANCE" eur)) (= 0 (call "ACCOUNT-BALANCE" usd)))
          (grade-fail "money moved before the condition was signalled"))
        (handler-bind ((error (lambda (c)
                                (when (typep c mismatch)
                                  (let ((r (find "CONVERT" (compute-restarts c)
                                                 :key (lambda (r) (symbol-name (restart-name r)))
                                                 :test #'string=)))
                                    (unless r (grade-fail "no CONVERT restart"))
                                    (invoke-restart r 1.1))))))
          (call "TRANSFER" eur usd 1000))
        (unless (and (= 9000 (call "ACCOUNT-BALANCE" eur)) (= 1100 (call "ACCOUNT-BALANCE" usd)))
          (grade-fail "convert: balances ~a / ~a, want 9000 / 1100"
                      (call "ACCOUNT-BALANCE" eur) (call "ACCOUNT-BALANCE" usd))))
      ;; same currency still works
      (let ((a (call "MAKE-ACCOUNT" "x")) (b (call "MAKE-ACCOUNT" "y")))
        (call "DEPOSIT" a 300)
        (call "TRANSFER" a b 100)
        (unless (= 100 (call "ACCOUNT-BALANCE" b)) (grade-fail "same-currency transfer")))
      ;; 3. statement
      (let ((a (call "MAKE-ACCOUNT" "s")))
        (call "DEPOSIT" a 10000 :date (utc 2025 11 3))
        (call "WITHDRAW" a 2550 :date (utc 2025 12 1) :memo "rent")
        (let ((lines (call "STATEMENT" a)))
          (unless (equal lines '("2025-11-03 100.00 EUR deposit" "2025-12-01 -25.50 EUR rent"))
            (grade-fail "statement: ~s" lines)))))
  (error (c) (grade-fail "behaviour: ~a" c)))

(format t "~&GRADE PASS~%")
(uiop:quit 0)
