(in-package #:ledger)

(defun month-key (universal-time)
  "\"2026-03\" for a universal time in March 2026 (UTC)."
  (multiple-value-bind (s m h d month year) (decode-universal-time universal-time 0)
    (declare (ignore s m h d))
    (format nil "~4,'0d-~2,'0d" year month)))

(defun %parse-month-key (key)
  (values (parse-integer key :end 4) (parse-integer key :start 5)))

(defun %next-month-key (key)
  (multiple-value-bind (year month) (%parse-month-key key)
    (if (= month 12)
        (format nil "~4,'0d-01" (1+ year))
        (format nil "~4,'0d-~2,'0d" year (1+ month)))))

(defun months-between (from-key to-key)
  "Month keys from FROM-KEY to TO-KEY inclusive."
  (loop for key = from-key then (%next-month-key key)
        collect key
        until (string= key to-key)))

(defun entries-in-month (account key)
  "Entries of ACCOUNT in month KEY, oldest first."
  (reverse (remove-if-not (lambda (e) (string= key (month-key (entry-date e))))
                          (account-history account))))

(defun month-net (account key)
  "Sum of ACCOUNT's entries in month KEY, in cents."
  (reduce #'+ (entries-in-month account key) :key #'entry-amount))

(defun average-entry (account key)
  "Mean entry amount of ACCOUNT in month KEY, rounded to the cent; 0 for a
month without entries."
  (let ((entries (entries-in-month account key)))
    (if (null entries)
        0
        (round (month-net account key) (length entries)))))

(defun monthly-report (account from-key to-key)
  "One plist per month from FROM-KEY to TO-KEY:
(:month key :count n :net cents :average cents)."
  (mapcar (lambda (key)
            (list :month key
                  :count (length (entries-in-month account key))
                  :net (month-net account key)
                  :average (average-entry account key)))
          (months-between from-key to-key)))

(defun report-total (report)
  "Sum of the :net values of REPORT."
  (reduce #'+ report :key (lambda (row) (getf row :net))))
