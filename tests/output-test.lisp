(in-package #:lisp-mcp/tests)

(defun many-defuns (count)
  (with-output-to-string (s)
    (dotimes (i count)
      (format s "(defun fn-~D (x) \"Doc ~D.\" (+ x ~D))~%~%" i i i))))

(defmacro with-small-limits (() &body body)
  `(let ((lisp-mcp::*max-output-lines* 20)
         (lisp-mcp::*max-output-chars* 4000)
         (lisp-mcp::*max-line-chars* 200))
     ,@body))

(defun stored-id (text)
  "The oN id a cut-short result names."
  (let ((at (search "Stored as o" text)))
    (and at (subseq text (+ at 10) (position #\: text :start (+ at 10))))))

(deftest small-results-are-unchanged
  (with-project (root)
    (write-project-file root "a.lisp" (many-defuns 3))
    (let ((outline (call "source-outline" "path" "a.lisp")))
      (ok (null (search "Stored as" outline)))
      (ok (search "No stored outputs" (call "output-read"))))))

(deftest long-results-are-cut-and-stored
  (with-project (root)
    (write-project-file root "a.lisp" (many-defuns 100))
    (with-small-limits ()
      (let* ((outline (call "source-outline" "path" "a.lisp"))
             (id (stored-id outline)))
        (ok (equal "o1" id))
        (ok (search "defun fn-0 (x)" outline) "head kept")
        (ok (search "defun fn-99 (x)" outline) "tail kept")
        (ok (null (search "defun fn-50 (x)" outline)) "middle cut")
        (ok (<= (count #\Newline outline) 21))
        (ok (search "o1  source-outline  101 lines" (call "output-read")))
        ;; paging
        (let ((page (call "output-read" "id" "o1" "offset" 50 "limit" 3)))
          (ok (search "lines 50-52 of 101" page))
          (ok (search "fn-48" page))
          (ok (search "[continue with offset=53]" page)))
        (ok (search "past the end" (call "output-read" "id" "o1" "offset" 500)))
        ;; search, with alternatives and context
        (let ((hits (call "output-read" "id" "o1" "grep" "FN-50 (| fn-70 (" "context" 1)))
          (ok (search "2 matching lines of 101" hits))
          (ok (search "fn-49" hits) "context line")
          (ok (search "--" hits) "groups split"))
        (multiple-value-bind (text error) (call "output-read" "id" "o9")
          (ok error)
          (ok (search "Kept (newest first" text)))
        ;; the same through resources
        (ok (search "lines 2-3 of 101" (resource-text "lisp-output://o1/lines/2-3")))
        (ok (search "1 matching line" (resource-text "lisp-output://o1/grep/fn-77 (")))
        (ok (search "defun fn-50 (x)" (resource-text "lisp-output://o1")))))))

(deftest long-lines-are-clipped
  (with-project (root)
    (let ((lisp-mcp::*max-line-chars* 100))
      (let ((text (lisp-mcp::bound-output "t" (format nil "~A~%" (make-string 500 :initial-element #\x)))))
        (ok (search "[+400 chars]" text))
        (ok (search "long lines cut" text)))
      (let ((grep (lisp-mcp::render-output-grep (lisp-mcp::find-output "o1") "x" 0)))
        (ok (search "…" grep))))))

(deftest edit-hints-and-lenient-arguments
  (with-project (root)
    (write-project-file root "src/demo.lisp" *demo*)
    ;; a locator that misses lists close matches
    (multiple-value-bind (text error) (call "edit-delete" "path" "src/demo.lisp" "locator" "defun frobb")
      (ok error)
      (ok (search "Close matches" text))
      (ok (search "defun frob" text)))
    (ok (search "4 top-level forms" (nth-value 0 (call "source-read" "path" "src/demo.lisp" "locator" "path:9"))))
    ;; a stale hash shows the form as it is now and the hash to retry with
    (let ((hash (let ((check (call "source-check" "path" "src/demo.lisp" "rules" "none")))
                  (string-right-trim '(#\) #\Newline) (subseq check (+ 5 (search "hash " check)))))))
      (multiple-value-bind (text error)
          (call "edit-replace" "path" "src/demo.lisp" "locator" "defun caller"
                "code" "(defun caller () (frob 2))" "expectedHash" "0000")
        (ok error "camelCase expectedHash is honoured")
        (ok (search "may already be applied" text))
        (ok (search "The form now reads (path:3)" text))
        (ok (search (format nil "retry with expected_hash ~A" hash) text)))
      ;; "false" as a string is false: this writes
      (ok (search "Wrote" (call "edit-replace" "path" "src/demo.lisp" "locator" "defun caller"
                                "code" "(defun caller () (frob 3))" "dry_run" "false"
                                "compile" t)))
      (ok (search "compile skipped: No live image"
                  (call "edit-replace" "path" "src/demo.lisp" "locator" "defun caller"
                        "code" "(defun caller () (frob 4))" "compile" t))))))
