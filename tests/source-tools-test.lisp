(in-package #:lisp-mcp/tests)

(defparameter *demo*
  "(defpackage #:demo (:use #:cl) (:export #:frob))
(in-package #:demo)

(defun frob (x)
  \"Frob X.\"
  (let ((unused 1))
    (+ x 1)))

(defun caller () (frob 2))
")

(deftest catalog-has-annotated-tools
  (with-project (root)
    (let ((tools (mcp:list-tools *server*)))
      (ok (>= (length tools) 40))
      (ok (find "edit-replace" tools :key #'mcp:mcp-tool-name :test #'string=))
      (let ((read (find "source-read" tools :key #'mcp:mcp-tool-name :test #'string=)))
        (ok (eq t (gethash "readOnlyHint" (mcp:mcp-tool-annotations read))))))))

(deftest outline-read-check
  (with-project (root)
    (write-project-file root "src/demo.lisp" *demo*)
    (let ((outline (call "source-outline" "path" "src/demo.lisp")))
      (ok (search "defun frob (x)" outline))
      (ok (search "path:2" outline)))
    (let ((form (call "source-read" "path" "src/demo.lisp" "locator" "defun caller")))
      (ok (search "(defun caller () (frob 2))" form)))
    (let ((check (call "source-check" "path" "src/demo.lisp")))
      (ok (search "unused-variable" check))
      (ok (search "UNUSED" check)))
    (multiple-value-bind (text error) (call "source-read" "path" "../outside.lisp")
      (ok error)
      (ok (search "outside the project root" text)))))

(defparameter *branches*
  "(in-package #:cl-user)

#+sbcl (defun impl () :sbcl)

#-sbcl (defun impl () :other)
")

(deftest reader-conditional-branches-survive
  "lisp-source-protocol#9: every #+/#- branch is outlined, located and written,
whatever the server's own features."
  (with-project (root)
    (write-project-file root "branches.lisp" *branches*)
    (let ((outline (call "source-outline" "path" "branches.lisp")))
      (ok (search "#+sbcl defun impl ()  [path:1.1]" outline))
      (ok (search "#-sbcl defun impl ()  [path:2.1]" outline)))
    (ok (search "#-sbcl (defun impl () :other)" (call "source-read" "path" "branches.lisp" "locator" "path:2")))
    (multiple-value-bind (text error) (call "source-read" "path" "branches.lisp" "locator" "defun impl")
      (ok error "two definitions")
      (ok (search "path:2.1  under #-sbcl" text)))
    (let ((dry (call "edit-insert" "path" "branches.lisp" "locator" "path:2"
                     "code" "#+sbcl (list :plus) #-sbcl (list :minus)" "dry_run" t)))
      (ok (search "+#+sbcl (list :plus)" dry))
      (ok (search "+#-sbcl (list :minus)" dry)))
    (call "edit-insert" "path" "branches.lisp" "locator" "path:2"
          "code" "#+sbcl (list :plus) #-sbcl (list :minus)")
    (call "edit-replace" "path" "branches.lisp" "locator" "path:2.1.3" "code" ":ecl")
    (call "edit-delete" "path" "branches.lisp" "locator" "path:1.1")
    (ok (equal (uiop:read-file-string (merge-pathnames "branches.lisp" root))
               (format nil "(in-package #:cl-user)~%~%#-sbcl (defun impl () :ecl)~%~%#+sbcl (list :plus)~%~%#-sbcl (list :minus)~%")))
    (ok (search "clean (4 top-level forms" (call "source-check" "path" "branches.lisp" "rules" "none")))))

(defparameter *commented-code*
  ";;; Section comment before the first inserted form.

(defun probe-b ()
  ;; inner comment
  2)

;;; Comment between forms.

(defun probe-c () 3)")

(deftest inserted-comments-reach-the-file
  "lisp-mcp#28 (lisp-source-protocol#11): edit-insert keeps the comments of its code."
  (with-project (root)
    (write-project-file root "probe.lisp" (format nil "(defun probe-a () 1)~%"))
    (let ((dry (call "edit-insert" "path" "probe.lisp" "locator" "defun probe-a"
                     "code" *commented-code* "dry_run" t "verbose" t)))
      (ok (search "+;;; Section comment before the first inserted form." dry))
      (ok (search "+;;; Comment between forms." dry)))
    (call "edit-insert" "path" "probe.lisp" "locator" "defun probe-a" "code" *commented-code*)
    (ok (equal (uiop:read-file-string (merge-pathnames "probe.lisp" root))
               (format nil "(defun probe-a () 1)~%~%~A~%" *commented-code*)))))

(defparameter *labels-file*
  "(defun outer (ret)
  (labels ((touch ()
             (setf ret 2)))
    (touch)))
")

(deftest local-definitions-keep-their-layout
  "lisp-source-protocol#14, #15: a binding inserted first goes on its own line,
COND clauses and LABELS bodies indent as SLIME does, and a trailing comment
goes after the closing parens."
  (with-project (root)
    (write-project-file root "outer.lisp" *labels-file*)
    (call "edit-insert" "path" "outer.lisp" "locator" "path:0.3.1.0" "where" "before"
          "code" (format nil "(route-hop ()~%\"Doc.\"~%(cond~%((plusp ret) :ok)~%(t :error)))"))
    (call "edit-replace" "path" "outer.lisp" "locator" "path:0.3.1.1"
          "code" (format nil "(touch ()~%(setf ret 3)) ; now 3"))
    (ok (equal (uiop:read-file-string (merge-pathnames "outer.lisp" root))
               "(defun outer (ret)
  (labels ((route-hop ()
             \"Doc.\"
             (cond
               ((plusp ret) :ok)
               (t :error)))
           (touch ()
             (setf ret 3))) ; now 3
    (touch)))
"))
    (ok (search "clean" (call "source-check" "path" "outer.lisp" "rules" "none")))))

(deftest broken-file-is-reported
  (with-project (root)
    (write-project-file root "broken.lisp" (format nil "(defun a (x)~%  (let ((y 1))~%    (+ x y))~%~%(defun b () 2)~%"))
    (let ((check (call "source-check" "path" "broken.lisp" "rules" "none")))
      (ok (search "imbalance missing-close" check))
      (ok (search "suggested fix at 3:" check)))
    (call "edit-structure" "op" "repair" "path" "broken.lisp")
    (ok (search "clean" (call "source-check" "path" "broken.lisp" "rules" "none")))))

(deftest structural-edits
  (with-project (root)
    (write-project-file root "src/demo.lisp" *demo*)
    (multiple-value-bind (text error)
        (call "edit-replace" "path" "src/demo.lisp" "locator" "defun caller" "code" "(defun caller () (frob 3")
      (ok error "unbalanced code is refused")
      (ok (search "balanced" text)))
    (let ((dry (call "edit-replace" "path" "src/demo.lisp" "locator" "defun caller"
                     "code" "(defun caller () (frob 3))" "dry_run" t)))
      (ok (search "Dry run" dry))
      ;; the operation and a real hash, not the path and the operation name
      (ok (search "Dry run, nothing written to src/demo.lisp: replace; new file hash " dry))
      (let ((at (+ (search "new file hash " dry) (length "new file hash "))))
        (ok (every (lambda (c) (digit-char-p c 16)) (subseq dry at (+ at 16)))))
      (ok (search "+(defun caller () (frob 3))" dry)))
    (ok (search "(frob 2)" (uiop:read-file-string (merge-pathnames "src/demo.lisp" root))))
    (multiple-value-bind (text error)
        (call "edit-replace" "path" "src/demo.lisp" "locator" "defun caller" "code" "(defun caller () 0)"
              "expected_hash" "stale")
      (ok error)
      (ok (search "re-read" text)))
    (call "edit-insert" "path" "src/demo.lisp" "locator" "defun caller" "code" "(defun added () :ok)")
    (call "edit-structure" "op" "wrap" "path" "src/demo.lisp" "locator" "path:3.3" "head" "progn")
    (call "edit-rename-symbol" "symbol" "demo:frob" "new_name" "twiddle")
    (call "edit-add-export" "path" "src/demo.lisp" "package" "demo" "symbol" "added")
    (let ((text (uiop:read-file-string (merge-pathnames "src/demo.lisp" root))))
      (ok (search "(:export #:twiddle #:added)" text))
      (ok (search "(defun twiddle (x)" text))
      (ok (search (format nil "(defun caller () (progn (twiddle 2)))~%~%(defun added () :ok)") text)))
    (ok (search "clean" (call "source-check" "path" "src/demo.lisp" "rules" "none")))
    (call "edit-new-file" "path" "src/new.lisp" "code" "(in-package #:demo)")
    (ok (probe-file (merge-pathnames "src/new.lisp" root)))
    (ok (nth-value 1 (call "edit-new-file" "path" "src/new.lisp" "code" "(x)")) "no overwrite")))

(deftest static-references
  (with-project (root)
    (write-project-file root "src/demo.lisp" *demo*)
    (ok (search "3 references to DEMO:FROB" (call "source-find-references" "symbol" "demo:frob")))
    (ok (search "defun FROB" (call "source-find-definitions" "symbol" "frob" "package" "demo")))))

(deftest locator-syntax
  (ok (equal (lisp-mcp:parse-locator "path:0.2.1") '(:path 0 2 1)))
  (ok (equal (lisp-mcp:parse-locator "line:12:4") '(:line 12 :column 4)))
  (ok (equal (lisp-mcp:parse-locator "defmethod print-object (my-class t)")
             '(:definition "defmethod" "print-object" "(my-class t)")))
  (ok (signals (lisp-mcp:parse-locator "nonsense") 'error)))

(deftest move-and-reindent
  (with-project (root)
    (write-project-file root "src/demo.lisp" *demo*)
    (write-project-file root "src/other.lisp" (format nil "(in-package #:demo)~%~%(defun z () 0)~%"))
    ;; within a file
    (call "edit-structure" "op" "move" "path" "src/demo.lisp" "locator" "defun caller" "target_locator" "defun frob" "where" "before")
    (let ((text (uiop:read-file-string (merge-pathnames "src/demo.lisp" root))))
      (ok (< (search "(defun caller" text) (search "(defun frob" text))))
    ;; across files: both are written, and both stay clean
    (call "edit-structure" "op" "move" "path" "src/demo.lisp" "locator" "defun caller" "target_path" "src/other.lisp"
          "target_locator" "defun z")
    (ok (not (search "defun caller" (uiop:read-file-string (merge-pathnames "src/demo.lisp" root)))))
    (ok (search (format nil "(defun z () 0)~%~%(defun caller () (frob 2))")
                (uiop:read-file-string (merge-pathnames "src/other.lisp" root))))
    (ok (search "clean" (call "source-check" "path" "src/other.lisp" "rules" "none")))
    ;; replaced code is re-indented
    (call "edit-replace" "path" "src/other.lisp" "locator" "defun z"
          "code" (format nil "(defun z ()~%(let ((a 1))~%(+ a~%1)))"))
    (ok (search (format nil "(defun z ()~%  (let ((a 1))~%    (+ a~%       1)))")
                (uiop:read-file-string (merge-pathnames "src/other.lisp" root))))))

(deftest project-macros-drive-indentation
  (with-project (root)
    (write-project-file root "src/macros.lisp"
                        (format nil "(in-package #:cl-user)~%~%(defmacro with-widget ((var) &body body) `(let ((,var 1)) ,@body))~%~%(defmacro define-thing2 (name kind spec &body body) (list name kind spec body))~%~%(defun use () nil)~%"))
    (call "edit-replace" "path" "src/macros.lisp" "locator" "defun use"
          "code" (format nil "(defun use ()~%(define-thing2 a b c~%(body)))"))
    (ok (search (format nil "(define-thing2 a b c~%    (body))")
                (uiop:read-file-string (merge-pathnames "src/macros.lisp" root))))))

(deftest packages-tool
  (with-project (root)
    (write-project-file root "src/demo.lisp" *demo*)
    (write-project-file root "src/pkg.lisp"
                        "(uiop:define-package #:demo.util (:use #:cl) (:nicknames #:du) (:local-nicknames (#:a #:alexandria)) (:export #:x #:y))")
    (let ((text (call "source-packages")))
      (ok (search "DEMO  src/demo.lisp:1:" text))
      (ok (search "DEMO.UTIL (nicknames DU)" text))
      (ok (search "2 exports" text))
      (ok (search "A=ALEXANDRIA" text))
      (ok (search "src/pkg.lisp:1:" text)))))

(deftest cache-sees-external-changes
  (with-project (root)
    (write-project-file root "a.lisp" "(defun one () 1)")
    (ok (search "defun one" (call "source-outline" "path" "a.lisp")))
    ;; a write from outside this server, same second, different length
    (write-project-file root "a.lisp" "(defun one () 1) (defun two () 2)")
    (ok (search "defun two" (call "source-outline" "path" "a.lisp")))))

(deftest edit-structure-ops
  (with-project (root)
    (flet ((text () (uiop:read-file-string (merge-pathnames "s.lisp" root)))
           (op (&rest kvs) (apply #'call "edit-structure" "path" "s.lisp" kvs)))
      (write-project-file root "s.lisp" (format nil "(defun f () (a (b c) d))~%"))
      (op "op" "slurp" "locator" "path:0.3.1")
      (ok (search "(a (b c d))" (text)) "slurp forward")
      (op "op" "barf" "locator" "path:0.3.1")
      (ok (search "(a (b c) d)" (text)) "barf forward")
      (op "op" "transpose" "locator" "path:0.3.1")
      (ok (search "(a d (b c))" (text)) "transpose")
      (op "op" "splice" "locator" "path:0.3.2")
      (ok (search "(a d b c)" (text)) "splice")
      (op "op" "raise" "locator" "path:0.3.1")
      (ok (search "(defun f () d)" (text)) "raise")
      (ok (signals (op "op" "twist" "locator" "path:0") 'mcp:mcp-error) "op is an enum")
      (multiple-value-bind (msg error) (op "op" "wrap")
        (ok error "wrap needs a locator")
        (ok (search "locator" msg))))))
