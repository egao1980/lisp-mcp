(in-package #:lisp-mcp)

;;; Structural edit tools. Every write re-reads the result and is refused if
;;; it would add a reader error or paren imbalance.

(defparameter +expected-hash+
  '("expected_hash" "string" "File hash from source-read; refused if the file changed"))
(defparameter +dry-run+ '("dry_run" "boolean" "Show the diff without writing"))

(defparameter +compile+
  '("compile" "boolean" "Compile the changed forms in the image; append its notes"))
(defparameter +edit-connection+ '("connection" "string" "Image for compile=true (default: the most recent)"))

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

(define-node-edit "edit-replace"
    "Replace one form with new code (exactly one balanced form; re-indented)."
    (parsed node args) ('("code" "string" "Replacement form" :required t))
  (src:replace-node parsed node (require-arg args "code")))

(define-node-edit "edit-insert"
    "Insert one or more forms before or after a form, as siblings (top-level forms get a blank line)."
    (parsed node args) ('("code" "string" "Forms to insert" :required t)
                        '("where" "string" "before or after (default after)" :enum ("before" "after")))
  (if (string-equal (arg-string args "where" "after") "before")
      (src:insert-before parsed node (require-arg args "code"))
      (src:insert-after parsed node (require-arg args "code"))))

(define-node-edit "edit-delete" "Delete a form and the whitespace that separated it." (parsed node args) ()
  (src:delete-node parsed node))

(define-node-edit "edit-wrap" "Wrap a form: (head FORM), e.g. head \"when (ready-p)\" or \"let ((x 1))\"; empty head gives (FORM)."
    (parsed node args) ('("head" "string" "Operator and leading arguments"))
  (src:wrap-node parsed node (let ((h (arg-string args "head"))) (and h (plusp (length h)) h))))

(define-node-edit "edit-splice" "Replace a list by its elements (remove one level of parens)." (parsed node args) ()
  (src:splice-node parsed node))

(define-node-edit "edit-raise" "Replace the form's parent list with the form." (parsed node args) ()
  (src:raise-node parsed node))

(define-node-edit "edit-slurp" "Grow a list: pull the next (forward) or previous (backward) sibling inside."
    (parsed node args) ('("direction" "string" "forward or backward (default forward)" :enum ("forward" "backward")))
  (if (string-equal (arg-string args "direction" "forward") "backward")
      (src:slurp-backward parsed node)
      (src:slurp-forward parsed node)))

(define-node-edit "edit-barf" "Shrink a list: push its last (forward) or first (backward) element out."
    (parsed node args) ('("direction" "string" "forward or backward (default forward)" :enum ("forward" "backward")))
  (if (string-equal (arg-string args "direction" "forward") "backward")
      (src:barf-backward parsed node)
      (src:barf-forward parsed node)))

(define-node-edit "edit-transpose" "Swap a form with its next sibling." (parsed node args) ()
  (src:transpose-nodes parsed node))

(define-tool "edit-move"
    (args :description "Move a form next to another one, in this file or target_path. Both files are verified before writing."
          :kind :write)
    (input-schema +path+ +locator+
                  '("target_locator" "string" "Form to move next to (same syntax as locator)" :required t)
                  '("target_path" "string" "File of the target (default: path)")
                  '("where" "string" "after (default) or before" :enum ("after" "before"))
                  +expected-hash+
                  '("target_expected_hash" "string" "Hash of the target file, when it is another file")
                  +dry-run+ +compile+ +edit-connection+)
  (multiple-value-bind (parsed name) (read-project-file (require-arg args "path"))
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
            (write-string (commit (second plans) args target-name :hash-arg "target_expected_hash") s)))))))

(define-tool "edit-repair"
    (args :description "Apply source-check's fix for a paren imbalance; kept only if the problem count drops."
          :kind :write)
    (input-schema +path+ '("index" "integer" "Which imbalance (0-based, default 0)") +expected-hash+ +dry-run+
                  +compile+ +edit-connection+)
  (multiple-value-bind (parsed name) (read-project-file (require-arg args "path"))
    (let ((imbalance (nth (arg-int args "index" 0) (src:parsed-source-imbalances parsed))))
      (unless imbalance
        (error 'bad-argument :message (format nil "~A has no imbalance ~D" name (arg-int args "index" 0))))
      (commit (src:repair-parens parsed :imbalance imbalance) args name))))

(define-tool "edit-rename-symbol"
    (args :description "Rename a symbol where it resolves (project or one file), defpackage designators included. Verifies every file."
          :kind :write)
    (input-schema +symbol+ +package+ '("new_name" "string" "New symbol name" :required t)
                  '("path" "string" "Limit to this file (default: whole project)") +dry-run+)
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
    (args :description "Add a symbol to a package's :export list in its defpackage (keeps the designator style)." :kind :write)
    (input-schema +path+ '("package" "string" "Package name" :required t) '("symbol" "string" "Symbol name" :required t)
                  +expected-hash+ +dry-run+)
  (multiple-value-bind (parsed name) (read-project-file (require-arg args "path"))
    (commit (src:add-export parsed (require-arg args "package") (require-arg args "symbol")) args name)))

(define-tool "edit-new-file"
    (args :description "Create a new .lisp file. The code must read cleanly; refuses to overwrite." :kind :write)
    (input-schema +path+ '("code" "string" "File contents" :required t))
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
