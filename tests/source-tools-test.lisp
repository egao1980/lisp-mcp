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
