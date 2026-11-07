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

(deftest broken-file-is-reported
  (with-project (root)
    (write-project-file root "broken.lisp" (format nil "(defun a (x)~%  (let ((y 1))~%    (+ x y))~%~%(defun b () 2)~%"))
    (let ((check (call "source-check" "path" "broken.lisp" "rules" "none")))
      (ok (search "imbalance missing-close" check))
      (ok (search "suggested fix at 3:" check)))
    (call "edit-repair" "path" "broken.lisp")
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
      (ok (search "+(defun caller () (frob 3))" dry)))
    (ok (search "(frob 2)" (uiop:read-file-string (merge-pathnames "src/demo.lisp" root))))
    (multiple-value-bind (text error)
        (call "edit-replace" "path" "src/demo.lisp" "locator" "defun caller" "code" "(defun caller () 0)"
              "expected_hash" "stale")
      (ok error)
      (ok (search "re-read" text)))
    (call "edit-insert" "path" "src/demo.lisp" "locator" "defun caller" "code" "(defun added () :ok)")
    (call "edit-wrap" "path" "src/demo.lisp" "locator" "path:3.3" "head" "progn")
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
    (ok (search "3 references to DEMO::FROB" (call "source-find-references" "symbol" "demo:frob")))
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
    (call "edit-move" "path" "src/demo.lisp" "locator" "defun caller" "target_locator" "defun frob" "where" "before")
    (let ((text (uiop:read-file-string (merge-pathnames "src/demo.lisp" root))))
      (ok (< (search "(defun caller" text) (search "(defun frob" text))))
    ;; across files: both are written, and both stay clean
    (call "edit-move" "path" "src/demo.lisp" "locator" "defun caller" "target_path" "src/other.lisp"
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
