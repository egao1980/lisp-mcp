(defpackage #:ledger/tests
  (:use #:cl #:rove #:ledger))

(in-package #:ledger/tests)

(defun utc (year month day)
  (encode-universal-time 0 0 12 day month year 0))

(deftest amounts
  (ok (= 1234 (parse-amount "12.34")))
  (ok (= 1230 (parse-amount "12.3")))
  (ok (= -500 (parse-amount "-5")))
  (ok (equal "12.34 EUR" (format-amount 1234 "EUR")))
  (ok (equal "-0.05 USD" (format-amount -5 "USD"))))

(deftest deposit-withdraw-transfer
  (let ((a (make-account "alice")) (b (make-account "bob")))
    (deposit a 10000)
    (withdraw a 2500)
    (ok (= 7500 (acct-bal a)))
    (transfer a b 5000)
    (ok (= 2500 (acct-bal a)))
    (ok (= 5000 (acct-bal b)))
    (ok (signals (withdraw b 9999) 'insufficient-funds))
    (handler-bind ((insufficient-funds #'continue))
      (withdraw b 6000))
    (ok (= -1000 (acct-bal b)) "CONTINUE overdraws")))

(deftest ledger-lookup
  (let ((l (make-ledger)))
    (add-account l (make-account "carol"))
    (add-account l (make-account "alice"))
    (ok (equal '("alice" "carol") (mapcar #'account-name (ledger-accounts l))))
    (ok (signals (find-account l "nobody") 'unknown-account))
    (let ((fallback (make-account "fallback")))
      (ok (eq fallback (handler-bind ((unknown-account (lambda (c) (use-value fallback c))))
                         (find-account l "nobody")))))))

(deftest reports
  (let ((a (make-account "alice")))
    (deposit a 10000 :date (utc 2025 11 3))
    (withdraw a 3000 :date (utc 2025 11 20))
    (deposit a 500 :date (utc 2026 1 15))
    (ok (equal "2025-11" (month-key (utc 2025 11 3))))
    (ok (= 7000 (month-net a "2025-11")))
    (ok (= 3500 (average-entry a "2025-11")))
    (let ((report (monthly-report a "2025-11" "2026-01")))
      (ok (equal '("2025-11" "2025-12" "2026-01") (mapcar (lambda (r) (getf r :month)) report)))
      (ok (= 0 (getf (second report) :count)) "an empty month is reported")
      (ok (= 0 (getf (second report) :average)))
      (ok (= 7500 (report-total report))))))
