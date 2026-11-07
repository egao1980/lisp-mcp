(in-package #:lisp-mcp)

;;; Compact text renderings for tool results.

(defmacro with-text ((stream) &body body)
  `(with-output-to-string (,stream) ,@body))

(defun position-string (parsed offset)
  (multiple-value-bind (line col) (src:offset-line-column (src:parsed-source-text parsed) offset)
    (format nil "~D:~D" line col)))

(defun path-string (node)
  (format nil "path:~{~D~^.~}" (src:node-path node)))

(defun render-diagnostics (parsed name &key (lint '()))
  (with-text (s)
    (let ((diags (append (src:parsed-source-diagnostics parsed) lint))
          (imbalances (src:parsed-source-imbalances parsed)))
      (if (and (null diags) (null imbalances))
          (format s "~A: clean (~D top-level forms, hash ~A)~%" name
                  (length (src:parsed-source-forms parsed)) (src:parsed-source-hash parsed))
          (progn
            (dolist (i imbalances)
              (format s "~A:~A: imbalance ~(~A~) x~D (~(~A~) confidence): ~A~@[; suggested fix at ~A~]~%"
                      name (position-string parsed (src:imbalance-form-start i))
                      (src:imbalance-kind i) (src:imbalance-count i) (src:imbalance-confidence i)
                      (src:imbalance-message i)
                      (and (src:imbalance-fix-position i)
                           (position-string parsed (src:imbalance-fix-position i)))))
            (dolist (d (sort (copy-list diags) #'< :key #'src:diagnostic-start))
              (format s "~A:~A: ~(~A~) ~(~A~): ~A~%" name (position-string parsed (src:diagnostic-start d))
                      (src:diagnostic-severity d) (src:diagnostic-kind d)
                      (substitute #\Space #\Newline (src:diagnostic-message d))))
            (when imbalances
              (format s "Use edit-repair to apply a suggested fix; forms marked recovered refuse other edits.~%")))))))

(defun render-outline (parsed name &key marks)
  "Outline text. MARKS, when given, has one entry per outline entry: T (the
image defines it), NIL (it does not) or :UNKNOWN."
  (with-text (s)
    (format s "~A (hash ~A, ~D forms~:[~;, has reader problems~])~%" name (src:parsed-source-hash parsed)
            (length (src:parsed-source-forms parsed)) (not (src:parsed-source-clean-p parsed)))
    (loop for e in (src:outline parsed)
          for i from 0
          for mark = (and marks (nth i marks))
          do (let ((node (src:outline-entry-node e)))
        (format s "  ~5@A  ~A ~A~@[ ~A~]  [~A]~@[ ~S~]~@[ ~A~]~%"
                (format nil "L~A" (src:offset-line-column (src:parsed-source-text parsed) (src:node-start node)))
                (src:outline-entry-kind e) (string-downcase (src:outline-entry-name e))
                (src:outline-entry-lambda-list e) (path-string node)
                (let ((doc (src:outline-entry-docstring e)))
                  (and doc (subseq doc 0 (min 70 (or (position #\Newline doc) (length doc))))))
                (and marks (case mark ((t) "[image: defined]") ((nil) "[image: not loaded]") (t nil))))))))

(defun render-node (parsed name node)
  (multiple-value-bind (l1) (src:offset-line-column (src:parsed-source-text parsed) (src:node-start node))
    (multiple-value-bind (l2) (src:offset-line-column (src:parsed-source-text parsed)
                                                      (max (src:node-start node) (1- (src:node-end node))))
      (format nil "~A ~A lines ~D-~D (file hash ~A)~:[~; [recovered]~]~%~A~%"
              name (path-string node) l1 l2 (src:parsed-source-hash parsed)
              (src:node-recovered-p node) (src:node-text node)))))

(defun render-edit-result (result name)
  (with-text (s)
    (format s "~:[Dry run, nothing written~;Wrote ~A~]: ~(~A~); new file hash ~A~%"
            (src:edit-result-written-p result) name
            (src:edit-plan-operation (src:edit-result-plan result))
            (src:parsed-source-hash (src:edit-result-parsed result)))
    (write-string (src:edit-result-diff result) s)))

;;; Live image

(defun render-debugger (state)
  (with-text (s)
    (format s "DEBUGGER level ~D (thread ~A): ~A~@[ [~A]~]~%Restarts:~%"
            (repl:debugger-state-level state) (repl:debugger-state-thread state)
            (repl:debugger-state-condition state) (repl:debugger-state-condition-type state))
    (dolist (r (repl:debugger-state-restarts state))
      (format s "  ~D: [~A] ~A~%" (repl:restart-info-index r) (repl:restart-info-name r)
              (repl:restart-info-description r)))
    (format s "Backtrace:~%")
    (dolist (f (subseq (repl:debugger-state-frames state)
                       0 (min 12 (length (repl:debugger-state-frames state)))))
      (format s "  ~D: ~A~%" (repl:frame-info-index f) (repl:frame-info-description f)))
    (format s "Next: debug-invoke-restart, debug-frame-locals, debug-eval-in-frame, inspect-condition.~%")))

(defun render-eval (result)
  (cond ((typep result 'repl:debugger-state) (render-debugger result))
        ((repl:eval-result-aborted-p result)
         (format nil "Aborted~@[: ~A~]~%" (repl:eval-result-reason result)))
        (t (with-text (s)
             (let ((out (repl:eval-result-output result)))
               (when (plusp (length out)) (format s "Output:~%~A~&" out)))
             (cond ((and (repl:eval-result-values result) (repl:eval-result-entry result))
                    (loop for v in (repl:eval-result-values result)
                          for i from 0
                          do (format s "=> ~A   #v~D:~D~%" v (repl:eval-result-entry result) i)))
                   ((repl:eval-result-values result)
                    (format s "=> ~{~A~^~%=> ~}~%" (repl:eval-result-values result)))
                   (t (format s "; No values~%")))
             (when (repl:eval-result-id result)
               (format s "(result id ~A; inspect it with inspect {\"result\": ~A})~%"
                       (repl:eval-result-id result) (repl:eval-result-id result)))))))

(defun render-inspector (view)
  (if (typep view 'repl:debugger-state)
      (render-debugger view)
      (with-text (s)
        (format s "~A~%" (repl:inspector-view-title view))
        (dolist (p (repl:inspector-view-parts view))
          (ecase (repl:inspector-part-kind p)
            (:text (write-string (repl:inspector-part-text p) s))
            (:value (format s "[~A]{~D}" (repl:inspector-part-text p) (repl:inspector-part-index p)))
            (:action (format s "<~A>{action ~D}" (repl:inspector-part-text p) (repl:inspector-part-index p)))))
        (format s "~&(inspect-part {\"index\": n} follows [value]{n}; inspect-action runs <action>)~%"))))

(defun render-location (loc)
  (cond ((null loc) "unknown location")
        ((repl:source-location-error loc) (format nil "(~A)" (repl:source-location-error loc)))
        (t (let ((file (repl:source-location-file loc))
                 (pos (repl:source-location-position loc)))
             (if (and file pos (probe-file file))
                 (let* ((parsed (cached-source file)))
                   (format nil "~A:~A" (relative-name file)
                           (position-string parsed (min pos (max 0 (1- (length (src:source-string (src:parsed-source-text parsed)))))))))
                 (format nil "~A~@[ @~D~]" file pos))))))

(defun render-compile (result &key (label "compile"))
  (if (typep result 'repl:debugger-state)
      (render-debugger result)
      (with-text (s)
        (format s "~A ~:[FAILED~;ok~] (~D note~:P)~:[~;, loaded~]~%" label (repl:compile-result-success-p result)
                (length (repl:compile-result-notes result)) (repl:compile-result-loaded-p result))
        (dolist (n (repl:compile-result-notes result))
          (format s "  ~(~A~) at ~A: ~A~%" (repl:compiler-note-severity n)
                  (render-location (repl:compiler-note-location n))
                  (substitute #\Space #\Newline (repl:compiler-note-message n)))))))

(defun render-xrefs (entries)
  (if (null entries)
      (format nil "No entries~%")
      (with-text (s)
        (dolist (e entries)
          (format s "~A  ~A~%" (repl:xref-entry-name e) (render-location (repl:xref-entry-location e)))))))
