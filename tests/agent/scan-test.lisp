(in-package #:lisp-mcp/agent/tests)

;;; FORM-RESOURCES golden cases (pure function, no image). Expected sets are
;;; compared as sets; "lisp:*" must be last, "lisp:pure" alone.

(deftest scan-blank
  (ok (null (scan "")))
  (ok (null (scan "   ")))
  (ok (null (scan (format nil "; just a comment~%"))))
  (ok (null (scan "#| block |#"))))

(deftest scan-skips-excluded-branches
  "A #+/#- branch the features exclude is source the image never runs, though
lisp-source-protocol 0.2.5 reads it as a form."
  (ok (null (scan "#+(or) (delete-file \"x\")")))
  (ok (null (scan "#-sbcl (delete-file \"x\")" :features '(:sbcl))))
  (ok (scan "#-sbcl (delete-file \"x\")" :features '(:ecl)) "included under other features")
  (check-scan '(("#+(or) #.(delete-file \"x\") 2" ("lisp:pure"))
                ("(list #+(or) #.(error \"x\") 1)" ("lisp:pure")))))

(deftest scan-pure
  (check-scan
   '(("(+ 1 2)" ("lisp:pure"))
     ("42" ("lisp:pure"))
     ("\"text\"" ("lisp:pure"))
     (":key" ("lisp:pure"))
     ("nil" ("lisp:pure"))
     ("x" ("lisp:pure"))
     ("(+ 1 2) (* 3 4)" ("lisp:pure"))
     ("(list 1 (car '(a b)) (length \"abc\"))" ("lisp:pure"))
     ("(cl:car '(1))" ("lisp:pure"))
     ("(common-lisp:car '(1))" ("lisp:pure"))
     ("'(setf *x* (delete-file \"x\"))" ("lisp:pure"))
     ("(quote (print 1))" ("lisp:pure"))
     ("(progn)" ("lisp:pure"))
     ("(if t 1 2)" ("lisp:pure"))
     ("(when t 1)" ("lisp:pure"))
     ("(unless nil 1)" ("lisp:pure"))
     ("(and 1 (or nil 2))" ("lisp:pure"))
     ("(cond ((= 1 1) :a) (t :b))" ("lisp:pure"))
     ("(case 1 (1 :a) (t :b))" ("lisp:pure"))
     ("(typecase 1 (integer :i) (t :x))" ("lisp:pure"))
     ("(block b (return-from b 1))" ("lisp:pure"))
     ("(the fixnum 1)" ("lisp:pure"))
     ("(locally (declare (optimize speed)) 1)" ("lisp:pure"))
     ("(values 1 2)" ("lisp:pure"))
     ("(let ((x 1) (y 2)) (* x y))" ("lisp:pure"))
     ("(let* ((x 1) (y (1+ x))) (list x y))" ("lisp:pure"))
     ("(let ((x 1)) (declare (fixnum x)) x)" ("lisp:pure"))
     ("(multiple-value-bind (q r) (floor 7 2) (list q r))" ("lisp:pure"))
     ("(destructuring-bind (a &key b) '(1 :b 2) (list a b))" ("lisp:pure"))
     ("(mapcar (lambda (x) (* x 2)) '(1 2 3))" ("lisp:pure"))
     ("(mapcar #'1+ '(1 2))" ("lisp:pure"))
     ("((lambda (x) (* x x)) 3)" ("lisp:pure"))
     ("(funcall #'+ 1 2)" ("lisp:pure"))
     ("(funcall 'list 1)" ("lisp:pure"))
     ("(apply #'max '(1 2))" ("lisp:pure"))
     ("(reduce #'+ '(1 2 3))" ("lisp:pure"))
     ("(flet ((sq (x) (* x x))) (sq 3))" ("lisp:pure"))
     ("(labels ((f (n) (if (zerop n) 1 (* n (f (1- n)))))) (f 5))" ("lisp:pure"))
     ("(dotimes (i 3) (1+ i))" ("lisp:pure"))
     ("(loop for x in '(1 2 3) collect (* x x))" ("lisp:pure"))
     ("(loop for (a b) in '((1 2)) collect (+ a b))" ("lisp:pure"))
     ("(loop for i from 0 below 10 sum i)" ("lisp:pure"))
     ("(handler-case (parse-integer \"x\") (error () nil))" ("lisp:pure"))
     ("(ignore-errors (parse-integer \"x\"))" ("lisp:pure"))
     ("(format nil \"~a\" 1)" ("lisp:pure"))
     ("(princ-to-string 1)" ("lisp:pure"))
     ("(prin1-to-string 'a)" ("lisp:pure"))
     ("(gethash :a (make-hash-table))" ("lisp:pure"))
     ("(find-symbol \"CAR\" \"CL\")" ("lisp:pure"))
     ("(find-package :cl)" ("lisp:pure"))
     ("(find-class 'standard-object)" ("lisp:pure"))
     ("(symbol-name 'foo)" ("lisp:pure"))
     ("(type-of 1)" ("lisp:pure"))
     ("(typep 1 'integer)" ("lisp:pure"))
     ("(subseq \"abc\" 1)" ("lisp:pure"))
     ("(concatenate 'string \"a\" \"b\")" ("lisp:pure"))
     ("(string-upcase \"a\")" ("lisp:pure"))
     ("(assoc :a '((:a . 1)))" ("lisp:pure"))
     ("(remove 1 '(1 2))" ("lisp:pure"))
     ("(position #\\a \"abc\")" ("lisp:pure"))
     ("(every #'evenp '(2 4))" ("lisp:pure"))
     ("(eval-when (:execute) (+ 1 2))" ("lisp:pure"))
     ("#+(or) (print 1) 2" ("lisp:pure"))
     ("#-(or) 3" ("lisp:pure"))
     ("`(a b)" ("lisp:pure"))
     ("`(a ,(+ 1 2))" ("lisp:pure")))))

(deftest scan-lexical-mutation-is-pure
  (check-scan
   '(("(let ((x 1)) (setf x 2) x)" ("lisp:pure"))
     ("(let ((x 1)) (setq x 2) (incf x) x)" ("lisp:pure"))
     ("(let ((acc '())) (dolist (x '(1 2 3)) (push x acc)) acc)" ("lisp:pure"))
     ("(let ((h (make-hash-table))) (setf (gethash :a h) 1) h)" ("lisp:pure"))
     ("(let* ((v (vector 1 2))) (setf (aref v 0) 3) v)" ("lisp:pure"))
     ("(let ((l (list 1 2))) (setf (car l) 0) l)" ("lisp:pure"))
     ("(let ((l (copy-list '(3 1 2)))) (sort l #'<))" ("lisp:pure"))
     ("(sort (copy-list '(3 1 2)) #'<)" ("lisp:pure"))
     ("(nreverse (list 1 2 3))" ("lisp:pure"))
     ("(let ((h (make-hash-table))) (remhash :a h))" ("lisp:pure"))
     ("(let ((*print-base* 16)) (princ-to-string 255))" ("lisp:pure"))
     ("(with-output-to-string (s) (format s \"x\") (write-string \"y\" s))" ("lisp:pure"))
     ("(with-input-from-string (s \"1 2\") (read-line s))" ("lisp:pure"))
     ("(let ((s (make-string-output-stream))) (write-string \"x\" s) (get-output-stream-string s))"
      ("lisp:pure")))))

(deftest scan-not-fresh-object-mutation-is-star
  (check-scan
   '(("(let ((h *table*)) (setf (gethash 1 h) 2))" ("lisp:reads cl-user:*table*" "lisp:*"))
     ("(let ((h (make-hash-table))) (setq h (foo)) (setf (gethash 1 h) 2))"
      ("lisp:calls cl-user:foo" "lisp:*"))
     ("(lambda (l) (setf (car l) 1))" ("lisp:*"))
     ("(sort (remove 1 '(3 1 2)) #'<)" ("lisp:*")))))

(deftest scan-reads
  (check-scan
   '(("*features*" ("lisp:reads cl:*features*"))
     ("(length *my-list*)" ("lisp:reads cl-user:*my-list*"))
     ("my-app::*state*" ("lisp:reads my-app:*state*"))
     ("(symbol-value '*x*)" ("lisp:reads cl-user:*x*"))
     ("(let ((*x* 1)) *x*)" ("lisp:pure"))
     ("(list *a* *b* *a*)" ("lisp:reads cl-user:*a*" "lisp:reads cl-user:*b*"))))
  (check-scan
   '(("(car *items*)" ("lisp:reads my-app:*items*"))
     ("(car cl-user::*items*)" ("lisp:reads cl-user:*items*")))
   :package "my-app")
  (check-scan '(("*x*" ("lisp:reads cl-user:*x*"))) :package "COMMON-LISP-USER"))

(deftest scan-defines
  (check-scan
   '(("(defun foo (x) x)" ("lisp:defines cl-user:foo"))
     ("(defmacro m (x) x)" ("lisp:defines cl-user:m"))
     ("(defvar *x* 1)" ("lisp:defines cl-user:*x*"))
     ("(defparameter *y* 2)" ("lisp:defines cl-user:*y*"))
     ("(defconstant +z+ 3)" ("lisp:defines cl-user:+z+"))
     ("(defclass point () ((x :initarg :x)))" ("lisp:defines cl-user:point"))
     ("(defstruct pt x y)" ("lisp:defines cl-user:pt"))
     ("(defstruct (pt2 (:conc-name p-)) x)" ("lisp:defines cl-user:pt2"))
     ("(defgeneric area (s))" ("lisp:defines cl-user:area"))
     ("(defmethod area ((s point)) 0)" ("lisp:defines cl-user:area"))
     ("(defun (setf foo) (v x) (declare (ignore x)) v)" ("lisp:defines (setf cl-user:foo)"))
     ("(define-condition oops (error) ())" ("lisp:defines cl-user:oops"))
     ("(deftype octet () '(unsigned-byte 8))" ("lisp:defines cl-user:octet"))
     ("(defun my-app::helper () 1)" ("lisp:defines my-app:helper"))
     ("(setf (fdefinition 'foo) #'car)" ("lisp:defines cl-user:foo"))
     ("(setf (symbol-function 'bar) (lambda () 1))" ("lisp:defines cl-user:bar"))
     ("(fmakunbound 'foo)" ("lisp:defines cl-user:foo"))
     ("(defun reset () (setf *count* 0))" ("lisp:defines cl-user:reset" "lisp:mutates cl-user:*count*"))
     ("(defun f () (unknown-helper))" ("lisp:defines cl-user:f" "lisp:calls cl-user:unknown-helper"))
     ("(defun g (x &optional (y (h))) (+ x y))" ("lisp:defines cl-user:g" "lisp:calls cl-user:h"))
     ("(defun f2 (h) (setf (gethash 1 h) 2))" ("lisp:defines cl-user:f2" "lisp:*"))
     ("(defun f3 () 1) (defun f4 () 2)" ("lisp:defines cl-user:f3" "lisp:defines cl-user:f4"))))
  (check-scan '(("(defun foo () 1)" ("lisp:defines my-app:foo"))) :package "my-app"))

(deftest scan-mutates
  (check-scan
   '(("(setf *x* 1)" ("lisp:mutates cl-user:*x*"))
     ("(setq *x* 1)" ("lisp:mutates cl-user:*x*"))
     ("(incf *counter*)" ("lisp:mutates cl-user:*counter*"))
     ("(push 1 *stack*)" ("lisp:mutates cl-user:*stack*"))
     ("(setf x 1)" ("lisp:mutates cl-user:x"))
     ("(setf (gethash :k *table*) 1)" ("lisp:mutates cl-user:*table*"))
     ("(setf (car *list*) 1)" ("lisp:mutates cl-user:*list*"))
     ("(setf (aref *v* 0) 1)" ("lisp:mutates cl-user:*v*"))
     ("(setf (slot-value *obj* 'x) 1)" ("lisp:mutates cl-user:*obj*"))
     ("(setf (getf *plist* :a) 1)" ("lisp:mutates cl-user:*plist*"))
     ("(remhash :k *table*)" ("lisp:mutates cl-user:*table*"))
     ("(clrhash *table*)" ("lisp:mutates cl-user:*table*"))
     ("(nreverse *list*)" ("lisp:mutates cl-user:*list*"))
     ("(sort *list* #'<)" ("lisp:mutates cl-user:*list*"))
     ("(nconc *a* (list 1))" ("lisp:mutates cl-user:*a*"))
     ("(rotatef *a* *b*)" ("lisp:mutates cl-user:*a*" "lisp:mutates cl-user:*b*"))
     ("(set '*x* 1)" ("lisp:mutates cl-user:*x*"))
     ("(makunbound '*x*)" ("lisp:mutates cl-user:*x*"))
     ("(setf (symbol-value '*x*) 1)" ("lisp:mutates cl-user:*x*"))
     ("(in-package :foo)" ("lisp:mutates cl:*package*"))
     ("(setf *x* (foo))" ("lisp:mutates cl-user:*x*" "lisp:calls cl-user:foo"))
     ("(setf (point-x *p*) 1)" ("lisp:calls (setf cl-user:point-x)" "lisp:mutates cl-user:*p*"))
     ("(setf my-app::*state* nil)" ("lisp:mutates my-app:*state*")))))

(deftest scan-io
  (check-scan
   '(("(print 1)" ("lisp:io"))
     ("(princ \"x\")" ("lisp:io"))
     ("(format t \"x\")" ("lisp:io"))
     ("(terpri)" ("lisp:io"))
     ("(write-line \"x\")" ("lisp:io"))
     ("(format *standard-output* \"x\")" ("lisp:io" "lisp:reads cl:*standard-output*"))
     ("(with-open-file (s \"/tmp/x\") (read-line s))" ("lisp:io"))
     ("(open \"/tmp/x\")" ("lisp:io"))
     ("(probe-file \"x\")" ("lisp:io"))
     ("(directory \"*.lisp\")" ("lisp:io"))
     ("(delete-file \"x\")" ("lisp:io"))
     ("(load \"x.lisp\")" ("lisp:io"))
     ("(compile-file \"x.lisp\")" ("lisp:io"))
     ("(describe 'car)" ("lisp:io"))
     ("(uiop:run-program \"ls\")" ("lisp:io"))
     ("(uiop:read-file-string \"x\")" ("lisp:io"))
     ("(eval-when (:execute) (print 1))" ("lisp:io"))
     ("(print 1) (setf *x* 2)" ("lisp:io" "lisp:mutates cl-user:*x*")))))

(deftest scan-read-eval
  (check-scan
   '(("#.(+ 1 2)" ("lisp:read-eval"))
     ("(list #.(delete-file \"x\"))" ("lisp:read-eval"))
     ("(read-from-string \"(1 2)\")" ("lisp:read-eval"))
     ("#S(point :x 1)" ("lisp:read-eval")))))

(deftest scan-calls
  (check-scan
   '(("(my-app:start)" ("lisp:calls my-app:start"))
     ("(foo 1)" ("lisp:calls cl-user:foo"))
     ("(MY-APP::Internal)" ("lisp:calls my-app:internal"))
     ("(|Foo| 1)" ("lisp:calls cl-user:|Foo|"))
     ("(asdf:load-system :foo)" ("lisp:calls asdf:load-system"))
     ("(funcall #'my-app:handler 1)" ("lisp:calls my-app:handler"))
     ("(funcall 'my-app:f)" ("lisp:calls my-app:f"))
     ("(mapcar #'my-app:f '(1))" ("lisp:calls my-app:f"))
     ("(foo (bar) *baz*)" ("lisp:calls cl-user:foo" "lisp:calls cl-user:bar" "lisp:reads cl-user:*baz*"))
     ("(#:foo)" ("lisp:calls #:foo"))
     ("`(a ,(foo) ,@*xs*)" ("lisp:calls cl-user:foo" "lisp:reads cl-user:*xs*"))))
  (check-scan '(("(foo 1)" ("lisp:calls my-app:foo"))) :package "MY-APP"))

(deftest scan-package
  (check-scan
   '(("(defpackage :my-app (:use :cl))" ("lisp:package my-app"))
     ("(defpackage #:my-app (:use #:cl))" ("lisp:package my-app"))
     ("(make-package \"FOO\")" ("lisp:package foo"))
     ("(delete-package :foo)" ("lisp:package foo"))
     ("(intern \"X\" :my-app)" ("lisp:package my-app"))
     ("(intern \"X\")" ("lisp:package cl-user"))
     ("(export 'foo)" ("lisp:package cl-user"))
     ("(uiop:define-package :bar (:use :cl))" ("lisp:package bar")))))

(deftest scan-not-modelled
  (check-scan
   '(("(funcall f 1)" ("lisp:*"))
     ("(let ((f #'car)) (funcall f '(1)))" ("lisp:*"))
     ("(eval '(+ 1 2))" ("lisp:*"))
     ("(macrolet ((m () 1)) (m))" ("lisp:*"))
     ("(symbol-macrolet ((x 1)) x)" ("lisp:*"))
     ("(progv '(*x*) '(1) *x*)" ("lisp:*"))
     ("(compile nil '(lambda () 1))" ("lisp:*"))
     ("(+ 1" ("lisp:*"))
     (")" ("lisp:*"))
     ("(1 2)" ("lisp:*"))
     ("(foo #<bar>)" ("lisp:*"))))
  (check-scan-includes
   '(("(apply (intern \"F\") nil)" "lisp:*")
     ("(print 1) (+ 1" "lisp:*")))
  (ok (equal '("lisp:*") (scan "(print 1) (+ 1")) "code that does not read cleanly is lisp:* alone"))

(deftest scan-deep-nesting-is-bounded
  (let ((deep (with-output-to-string (s)
                (dotimes (i 300) (write-string "(list " s))
                (write-string "1" s)
                (dotimes (i 300) (write-char #\) s)))))
    (ok (member "lisp:*" (scan deep) :test #'string=))))

(defun %lcg-strings (count seed)
  "COUNT pseudo-random strings over a Lisp-ish alphabet (fixed LCG)."
  (let ((alphabet "()'`,@#.:|\\\"; abcxyz*+-123setfquoeval
")
        (x seed) (out '()))
    (dotimes (i count (nreverse out))
      (let ((len (1+ (mod (setf x (mod (+ (* x 1103515245) 12345) 2147483648)) 40))))
        (push (coerce (loop repeat len
                            collect (char alphabet
                                          (mod (setf x (mod (+ (* x 1103515245) 12345) 2147483648))
                                               (length alphabet))))
                      'string)
              out)))))

(deftest scan-never-signals
  (let ((bad '()))
    (dolist (s (append (%lcg-strings 600 7)
                       (loop for c in '("(defun f (x) (setf *y* x))" "`(a ,(foo) ,@*xs*)"
                                        "(with-output-to-string (s) (format s \"x\"))"
                                        "#.(+ 1 2)" "(loop for (a b) in '((1 2)) collect a)")
                             append (loop for i from 0 to (length c) collect (subseq c 0 i)))))
      (unless (handler-case (well-formed-p (scan s))
                (error () nil))
        (push s bad)))
    (ok (null bad) (format nil "every string scans to a well-formed list; first bad: ~S" (first bad)))))
