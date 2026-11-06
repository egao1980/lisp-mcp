(in-package #:lisp-mcp)

;;; JSON Schema for tool inputs, and argument access.

(defun input-schema (&rest properties)
  "PROPERTIES: (name type description &key required enum default)."
  (let ((props (make-hash-table :test #'equal))
        (required '()))
    (dolist (p properties)
      (destructuring-bind (name type description &key required enum default) p
        (setf (gethash name props)
              (mcp:json-object "type" type
                               "description" description
                               "enum" (if enum (coerce enum 'vector) :omit)
                               "default" (if default default :omit)))
        (when required (push name required))))
    (mcp:json-object "type" "object"
                     "properties" props
                     "required" (if required (coerce (nreverse required) 'vector) :omit))))

(defun json-false-p (v)
  (or (null v) (member v '(:false :null :json-false))))

(defun arg (args name &optional default)
  (let ((v (mcp:param args name :absent)))
    (if (or (eq v :absent) (member v '(:null))) default v)))

(defun arg-bool (args name &optional default)
  (let ((v (arg args name :absent)))
    (cond ((eq v :absent) default)
          ((json-false-p v) nil)
          (t t))))

(defun arg-int (args name &optional default)
  (let ((v (arg args name)))
    (cond ((null v) default)
          ((integerp v) v)
          ((and (realp v) (= v (round v))) (round v))
          ((stringp v) (or (parse-integer v :junk-allowed t)
                           (error 'bad-argument :message (format nil "~A must be an integer" name))))
          (t (error 'bad-argument :message (format nil "~A must be an integer" name))))))

(defun arg-string (args name &optional default)
  (let ((v (arg args name)))
    (cond ((null v) default)
          ((stringp v) v)
          (t (princ-to-string v)))))

(defun require-arg (args name)
  (or (arg-string args name)
      (error 'bad-argument :message (format nil "Missing required argument ~A" name))))

;;; Locators

(defun parse-locator (string)
  "Locator syntax (documented in the tool descriptions):
  defun foo | defmethod print-object (my-class t) | defclass bar  → by definition
  path:0.2.1   → structural path (top-level index, then child indices)
  line:12 | line:12:4   → innermost form at line (1-based) / column (0-based)
  offset:120   → innermost form containing character offset
  hash:abcd…   → top-level form whose text hash matches"
  (let* ((s (string-trim " " string))
         (colon (position #\: s)))
    (flet ((ints (text sep)
             (mapcar (lambda (part)
                       (or (parse-integer part :junk-allowed t)
                           (error 'bad-argument :message (format nil "Bad locator ~S" string))))
                     (uiop:split-string text :separator sep))))
      (cond ((and colon (string-equal (subseq s 0 colon) "path"))
             (cons :path (ints (subseq s (1+ colon)) ".")))
            ((and colon (string-equal (subseq s 0 colon) "line"))
             (destructuring-bind (line &optional (column 0)) (ints (subseq s (1+ colon)) ":")
               (list :line line :column column)))
            ((and colon (string-equal (subseq s 0 colon) "offset"))
             (list :offset (first (ints (subseq s (1+ colon)) ","))))
            ((and colon (string-equal (subseq s 0 colon) "hash"))
             (list :hash (subseq s (1+ colon))))
            (t
             (let* ((space (position #\Space s))
                    (op (and space (subseq s 0 space)))
                    (rest (and space (string-trim " " (subseq s space))))
                    (paren (and rest (position #\( rest))))
               (unless (and op (plusp (length rest)))
                 (error 'bad-argument
                        :message (format nil "Bad locator ~S: use e.g. \"defun foo\", \"path:0.2\", \"line:12\"" string)))
               (if paren
                   (list :definition op (string-trim " " (subseq rest 0 paren)) (subseq rest paren))
                   (list :definition op rest))))))))
