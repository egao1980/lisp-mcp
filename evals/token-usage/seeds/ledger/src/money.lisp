(in-package #:ledger)

(define-condition ledger-error (error) ()
  (:documentation "Root of the ledger's errors."))

(define-condition unknown-account (ledger-error)
  ((name :initarg :name :reader unknown-account-name))
  (:report (lambda (c s)
             (format s "No account named ~s." (unknown-account-name c)))))

(define-condition insufficient-funds (ledger-error)
  ((account :initarg :account :reader insufficient-funds-account)
   (needed :initarg :needed :reader insufficient-funds-needed))
  (:report (lambda (c s)
             (format s "Account ~s lacks ~a."
                     (account-name (insufficient-funds-account c))
                     (format-amount (insufficient-funds-needed c)
                                    (account-currency (insufficient-funds-account c)))))))

(defun parse-amount (text)
  "Integer cents for TEXT such as \"12\", \"12.3\" or \"-12.34\"."
  (check-type text string)
  (let* ((text (string-trim " " text))
         (negative (and (plusp (length text)) (char= (char text 0) #\-)))
         (body (if negative (subseq text 1) text))
         (dot (position #\. body))
         (whole (parse-integer body :end dot))
         (fraction (if dot
                       (let ((digits (subseq body (1+ dot))))
                         (cond ((zerop (length digits)) 0)
                               ((= 1 (length digits)) (* 10 (parse-integer digits)))
                               ((= 2 (length digits)) (parse-integer digits))
                               (t (error "Too many decimals in ~s" text))))
                       0))
         (cents (+ (* 100 whole) fraction)))
    (if negative (- cents) cents)))

(defun format-amount (cents currency)
  "\"12.34 EUR\" for 1234 and \"EUR\"."
  (multiple-value-bind (whole fraction) (truncate (abs cents) 100)
    (format nil "~:[~;-~]~d.~2,'0d ~a" (minusp cents) whole fraction currency)))
