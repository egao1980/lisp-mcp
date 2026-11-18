(in-package #:ledger)

(defstruct entry
  (date (error "date required") :type (integer 0))   ; universal time
  (amount 0 :type integer)                           ; cents, signed
  (memo "" :type string))

(defclass account ()
  ((name :initarg :name :reader account-name)
   (currency :initarg :currency :reader account-currency :initform "EUR")
   (balance :initform 0 :accessor %balance)
   (history :initform '() :accessor account-history))
  (:documentation "An account in one currency. HISTORY is newest first."))

(defmethod print-object ((a account) stream)
  (print-unreadable-object (a stream :type t)
    (format stream "~a ~a" (account-name a) (format-amount (%balance a) (account-currency a)))))

(defun make-account (name &key (currency "EUR"))
  (make-instance 'account :name name :currency currency))

(defun acct-bal (account)
  "Balance of ACCOUNT in cents."
  (%balance account))

(defun %record (account amount date memo)
  (push (make-entry :date date :amount amount :memo memo) (account-history account))
  (incf (%balance account) amount))

(defun deposit (account cents &key (date (get-universal-time)) (memo "deposit"))
  (check-type cents (integer 1))
  (%record account cents date memo)
  account)

(defun withdraw (account cents &key (date (get-universal-time)) (memo "withdrawal"))
  "Take CENTS from ACCOUNT. Without enough money, signals INSUFFICIENT-FUNDS
with a CONTINUE restart that overdraws anyway."
  (check-type cents (integer 1))
  (when (> cents (%balance account))
    (cerror "Overdraw the account." 'insufficient-funds
            :account account :needed (- cents (%balance account))))
  (%record account (- cents) date memo)
  account)

(defun transfer (from to cents &key (date (get-universal-time)) (memo "transfer"))
  "Move CENTS from FROM to TO."
  (withdraw from cents :date date :memo (format nil "~a to ~a" memo (account-name to)))
  (deposit to cents :date date :memo (format nil "~a from ~a" memo (account-name from)))
  (values from to))

(defstruct (ledger (:constructor make-ledger ()))
  (table (make-hash-table :test #'equal) :type hash-table))

(defun ledger-accounts (ledger)
  "The accounts of LEDGER, sorted by name."
  (sort (loop for a being the hash-values of (ledger-table ledger) collect a)
        #'string< :key #'account-name))

(defun add-account (ledger account)
  (setf (gethash (account-name account) (ledger-table ledger)) account))

(defun find-account (ledger name)
  "The account called NAME. Signals UNKNOWN-ACCOUNT with a USE-VALUE restart."
  (or (gethash name (ledger-table ledger))
      (restart-case (error 'unknown-account :name name)
        (use-value (account)
          :report "Use another account."
          account))))
