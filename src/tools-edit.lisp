(in-package #:lisp-mcp)

;;; Structural edit tools. Every write re-reads the result and is refused if
;;; it would add a reader error or paren imbalance.

(defparameter +expected-hash+ '("expected_hash" "string" nil))
(defparameter +dry-run+ '("dry_run" "boolean" nil))
(defparameter +compile+ '("compile" "boolean" nil))
(defparameter +edit-connection+ '("connection" "string" nil))

(defun squeeze-space (string)
  (format nil "~{~A~^ ~}"
          (remove "" (uiop:split-string string :separator '(#\Space #\Tab #\Newline #\Return))
                  :test #'string=)))

(defun first-lines (text count)
  (let ((lines (uiop:split-string text :separator '(#\Newline))))
    (if (<= (length lines) count)
        text
        (format nil "~{~A~%~}… (~D more lines)" (subseq lines 0 count) (- (length lines) count)))))

(defun stale-hint (condition parsed name node args)
  "Message for a stale expected_hash: whether the edit looks already applied,
and the form as it now reads, so the retry needs no re-read."
  (with-text (s)
    (format s "~A changed since it was read: its hash is now ~A, expected_hash was ~A."
            name (src:stale-source-actual condition) (src:stale-source-expected condition))
    (let ((code (arg-string args "code")))
      (when (and code (plusp (length (squeeze-space code)))
                 (search (squeeze-space code) (squeeze-space (src:source-string (src:parsed-source-text parsed)))))
        (format s " The new code is already in the file: this edit may already be applied.")))
    (when node
      (format s "~%The form now reads (~A):~%~A" (path-string node) (first-lines (src:node-text node) 40)))
    (format s "~%If that is still the form you mean, retry with expected_hash ~A; otherwise re-read it with source-read.~%"
            (src:parsed-source-hash parsed))))

(defun changed-top-level-forms (plan result)
  "Top-level forms of RESULT's new text that the edits of PLAN wrote into."
  (let ((delta 0) (spans '()))
    (dolist (e (sort (copy-list (src:edit-plan-edits plan)) #'< :key #'src:text-edit-start))
      (let ((start (+ (src:text-edit-start e) delta))
            (size (length (src:text-edit-replacement e))))
        (push (cons start (+ start size)) spans)
        (incf delta (- size (- (src:text-edit-end e) (src:text-edit-start e))))))
    (remove-if-not (lambda (form)
                     (some (lambda (span) (and (< (car span) (src:node-end form))
                                               (> (cdr span) (src:node-start form))))
                           spans))
                   (src:parsed-source-forms (src:edit-result-parsed result)))))

(defun compile-changed (plan result args)
  "Compiler notes for the forms PLAN changed, compiled in the image; a note
when there is no image. Stops at a debugger entry."
  (let ((forms (changed-top-level-forms plan result))
        (parsed (src:edit-result-parsed result)))
    (if (null forms)
        ""
        (handler-case
            (let ((c (find-connection (arg-string args "connection"))))
              (with-text (s)
                (dolist (form forms)
                  (let ((compiled (repl:repl-compile-string c (src:node-text form)
                                                            :package (src:package-at parsed (src:node-start form))
                                                            :filename (namestring (src:parsed-source-path parsed))
                                                            :position (src:node-start form))))
                    (write-string (render-compile compiled :label (format nil "compiled ~A" (path-string form))) s)
                    (when (typep compiled 'repl:debugger-state) (return))))))
          ((or no-connection unknown-connection) (e)
            (format nil "compile skipped: ~A~%" e))))))

(defun commit (plan args name &key (hash-arg "expected_hash") node)
  "Write PLAN (or show it, with dry_run) and render the result. A stale hash
becomes a BAD-ARGUMENT with a retry hint; compile=true appends compiler notes."
  (let* ((parsed (src:edit-plan-parsed plan))
         (result (handler-bind ((src:stale-source
                                  (lambda (c) (error 'bad-argument :message (stale-hint c parsed name node args)))))
                   (src:commit-plan plan :dry-run (arg-bool args "dry_run")
                                         :expected-hash (arg-string args hash-arg)))))
    ;; the write may land in the same second with the same length
    (forget-source (src:parsed-source-path parsed))
    (with-text (s)
      (write-string (render-edit-result result name) s)
      (when (and (src:edit-result-written-p result) (arg-bool args "compile"))
        (write-string (compile-changed plan result args) s)))))

(defun indented-for-project (function)
  "Call FUNCTION with indentation prepared from the image and project macros."
  (prepare-indentation (project-sources :progress-message "Reading"))
  (funcall function))

(defmacro define-node-edit (tool-name description (parsed node args) extra-schema &body plan-form)
  `(define-tool ,tool-name (,args :description ,description :kind :write)
       (input-schema +path+ +locator+ ,@extra-schema +expected-hash+ +dry-run+ +compile+ +edit-connection+)
     (multiple-value-bind (,parsed name) (read-project-file (require-arg ,args "path"))
       (let ((,node (locate-in ,parsed (require-arg ,args "locator"))))
         (commit (indented-for-project (lambda () ,@plan-form)) ,args name :node ,node)))))

(define-node-edit "edit-replace" "Replace one form with new code (one balanced form; re-indented)."
    (parsed node args) ('("code" "string" "Replacement form" :required t))
  (src:replace-node parsed node (require-arg args "code")))

(define-node-edit "edit-insert" "Insert forms before or after a form, as siblings."
    (parsed node args) ('("code" "string" "Forms to insert" :required t)
                        '("where" "string" "Default after" :enum ("before" "after")))
  (if (string-equal (arg-string args "where" "after") "before")
      (src:insert-before parsed node (require-arg args "code"))
      (src:insert-after parsed node (require-arg args "code"))))

(define-node-edit "edit-delete" "Delete a form and the whitespace that separated it." (parsed node args) ()
  (src:delete-node parsed node))

;;; edit-structure: the rarer structural edits behind one tool and an op.

(defparameter *structure-ops*
  '("wrap" "splice" "raise" "slurp" "barf" "transpose" "move" "repair"))

(defun backward-p (args)
  (string-equal (arg-string args "direction" "forward") "backward"))

(defun node-plan (op parsed node args)
  "Edit plan of the one-form OP on NODE of PARSED."
  (alexandria:switch (op :test #'string-equal)
    ("wrap" (src:wrap-node parsed node (let ((h (arg-string args "head"))) (and h (plusp (length h)) h))))
    ("splice" (src:splice-node parsed node))
    ("raise" (src:raise-node parsed node))
    ("slurp" (if (backward-p args) (src:slurp-backward parsed node) (src:slurp-forward parsed node)))
    ("barf" (if (backward-p args) (src:barf-backward parsed node) (src:barf-forward parsed node)))
    ("transpose" (src:transpose-nodes parsed node))))

(defun move-form (parsed name args)
  "op move: NODE next to target_locator, in this file or target_path. Both
files are verified before either is written."
  (multiple-value-bind (target-parsed target-name)
      (let ((tp (arg-string args "target_path")))
        (if (and tp (not (equal (namestring (resolve-project-path tp))
                                (namestring (src:parsed-source-path parsed)))))
            (read-project-file tp)
            (values parsed name)))
    (let* ((node (locate-in parsed (require-arg args "locator")))
           (target (locate-in target-parsed (require-arg args "target_locator")))
           (plans (indented-for-project
                   (lambda ()
                     (src:move-node parsed node target-parsed target
                                    :where (if (string-equal (arg-string args "where" "after") "before")
                                               :before :after))))))
      ;; verify everything before writing anything
      (mapc #'src:verify-plan plans)
      (with-text (s)
        (write-string (commit (first plans) args name) s)
        (when (second plans)
          (write-string (commit (second plans) args target-name :hash-arg "target_expected_hash") s))))))

(defun repair-file (parsed name args)
  "op repair: apply source-check's fix for imbalance index; kept only if the
problem count drops."
  (let ((imbalance (nth (arg-int args "index" 0) (src:parsed-source-imbalances parsed))))
    (unless imbalance
      (error 'bad-argument :message (format nil "~A has no imbalance ~D" name (arg-int args "index" 0))))
    (commit (src:repair-parens parsed :imbalance imbalance) args name)))

(define-tool "edit-structure"
    (args :description "Structural edit by op: wrap, splice, raise, slurp, barf, transpose, move, repair."
          :kind :write)
    (input-schema '("op" "string" nil :required t :enum ("wrap" "splice" "raise" "slurp" "barf" "transpose" "move" "repair"))
                  +path+ '("locator" "string" "Not for repair")
                  '("head" "string" "wrap: operator and first args")
                  '("direction" "string" "slurp/barf: default forward" :enum ("forward" "backward"))
                  '("target_locator" "string" "move: next to this form")
                  '("target_path" "string" "move: its file (default path)")
                  '("where" "string" "move: default after" :enum ("after" "before"))
                  '("target_expected_hash" "string" "move: target file hash")
                  '("index" "integer" "repair: imbalance (default 0)")
                  +expected-hash+ +dry-run+ +compile+ +edit-connection+)
  (let ((op (require-arg args "op")))
    (unless (member op *structure-ops* :test #'string-equal)
      (error 'bad-argument :message (format nil "Unknown op ~S; one of ~{~A~^, ~}" op *structure-ops*)))
    (multiple-value-bind (parsed name) (read-project-file (require-arg args "path"))
      (alexandria:switch (op :test #'string-equal)
        ("move" (move-form parsed name args))
        ("repair" (repair-file parsed name args))
        (t (let ((node (locate-in parsed (require-arg args "locator"))))
             (commit (indented-for-project (lambda () (node-plan op parsed node args))) args name :node node)))))))

(define-tool "edit-rename-symbol"
    (args :description "Rename a symbol where it resolves, defpackage designators included."
          :kind :write)
    (input-schema +symbol+ +package+ '("new_name" "string" nil :required t)
                  '("path" "string" "Only this file") +dry-run+)
  (multiple-value-bind (package name) (symbol-args args)
    (let* ((all (project-sources))
           (scope (let ((p (arg-string args "path")))
                    (if p
                        (let ((file (namestring (resolve-project-path p))))
                          (remove file all :key #'src:parsed-source-path :test-not #'string=))
                        all)))
           (model (project-model all))
           (plans (src:rename-symbol model package name (require-arg args "new_name") :sources scope)))
      (if (null plans)
          (format nil "No references to ~A::~A~%" package name)
          ;; verify everything before writing anything
          (progn
            (mapc #'src:verify-plan plans)
            (with-text (s)
              (dolist (plan plans)
                (write-string (commit plan args (relative-name (src:parsed-source-path (src:edit-plan-parsed plan)))) s))))))))

(define-tool "edit-add-export"
    (args :description "Add a symbol to a defpackage :export list." :kind :write)
    (input-schema +path+ '("package" "string" nil :required t) '("symbol" "string" nil :required t)
                  +expected-hash+ +dry-run+)
  (multiple-value-bind (parsed name) (read-project-file (require-arg args "path"))
    (commit (src:add-export parsed (require-arg args "package") (require-arg args "symbol")) args name)))

(define-tool "edit-new-file"
    (args :description "Create a .lisp file; the code must read cleanly. Never overwrites." :kind :write)
    (input-schema +path+ '("code" "string" nil :required t))
  (let* ((file (resolve-project-path (require-arg args "path") :must-exist nil))
         (code (require-arg args "code")))
    (when (probe-file file)
      (error 'bad-argument :message (format nil "~A exists; edit it instead" (relative-name file))))
    (src:read-edit-input code :min 0)
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists nil :external-format :utf-8)
      (unless out
        (error 'bad-argument :message (format nil "~A exists; edit it instead" (relative-name file))))
      (write-string code out)
      (when (and (plusp (length code)) (char/= (char code (1- (length code))) #\Newline))
        (terpri out)))
    (forget-source file)
    (format nil "Created ~A~%" (relative-name file))))
