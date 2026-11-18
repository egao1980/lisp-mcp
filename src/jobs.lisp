(in-package #:lisp-mcp)

;;; Background jobs. A call to one of *JOB-TOOLS* runs in a worker thread;
;;; the call waits up to its timeout for it. If the work outlasts that, the
;;; remote evaluation keeps running (the worker keeps waiting on the wire),
;;; the call returns a job id, and the result is reported with a log message
;;; when it is done and kept for job-status / job-wait.

(defparameter *job-tools*
  '(("repl-eval" . 60) ("repl-compile" . 60) ("repl-load-system" . 300)
    ("repl-run-tests" . 600) ("debug-invoke-restart" . 60) ("debug-step" . 60))
  "Tools that continue as a job when they outlast their timeout argument, with
its default in seconds.")

(defparameter *job-key-ignored-arguments* '("timeout" "verbose")
  "Arguments that do not make two calls different jobs.")

(defvar *job-wait-limit* 300 "Most seconds one job-wait call blocks.")
(defvar *finished-jobs-kept* 32 "Finished jobs kept for job-status; older ones are dropped.")

(define-condition job-error (lisp-mcp-error) ())

(define-condition unknown-job (job-error bad-argument) ()
  (:documentation "No job with the id the client gave."))

(defclass job ()
  ((id :initarg :id :reader job-id)
   (tool :initarg :tool :reader job-tool)
   (key :initarg :key :reader job-key
        :documentation "Tool and arguments: a running job with the same key is not started again.")
   (connection :initarg :connection :initform nil :reader job-connection
               :documentation "Connection the job runs on, for job-cancel.")
   (thread :initarg :thread :initform nil :reader job-interrupt-thread
           :documentation "Image thread to interrupt (a channel's), or NIL for the newest worker.")
   (started :initform (get-internal-real-time) :reader job-started)
   (finished :initform nil :accessor job-finished
             :documentation "Internal real time the job ended, or NIL while it runs.")
   (state :initform :running :accessor job-state :type (member :running :done :failed))
   (result :initform nil :accessor job-result
           :documentation "Result text (already bounded), or the report of the error that ended the job.")
   (condition :initform nil :accessor job-condition
              :documentation "The error that ended the job, re-signalled to a caller still waiting.")
   (detached :initform nil :accessor job-detached
             :documentation "True once a call returned this job's id instead of its result.")))

(defun job-tool-p (name) (assoc name *job-tools* :test #'string=))

(defun job-running-p (job) (eq (job-state job) :running))

(defun job-seconds (job)
  (round (- (or (job-finished job) (get-internal-real-time)) (job-started job))
         internal-time-units-per-second))

(defun job-arguments-key (tool args)
  "TOOL and its arguments, sorted, as text; *JOB-KEY-IGNORED-ARGUMENTS* left out."
  (let ((pairs (and (hash-table-p args)
                    (loop for k being the hash-keys of args using (hash-value v)
                          unless (member k *job-key-ignored-arguments* :test #'string=)
                            collect (cons k v)))))
    (format nil "~A~{ ~A=~S~}" tool
            (loop for (k . v) in (sort pairs #'string< :key #'car) append (list k v)))))

(defun job-summary (job)
  "The key, short: what the job is."
  (let ((key (job-key job)))
    (if (> (length key) 100) (format nil "~A…" (subseq key 0 100)) key)))

(defun find-job (id &key (state *state*))
  (bt2:with-lock-held ((state-lock state))
    (or (find (string-trim " " id) (state-jobs state) :key #'job-id :test #'string-equal)
        (error 'unknown-job
               :message (format nil "No job ~S. Jobs: ~:[none~;~:*~{~A~^, ~}~]"
                                id (mapcar #'job-id (state-jobs state)))))))

(defun running-text (job)
  (format nil "Still running as job ~A (~D s); do not start another copy while it is running; the result is reported when done (or call job-status / job-wait).~%"
          (job-id job) (job-seconds job)))

(defun job-connection-of (args)
  "→ (values connection thread) the call ARGS will run on, NIL when unknown yet
(the handler then signals why)."
  (let ((channel (arg-string args "channel")))
    (cond (channel
           (let ((ch (bt2:with-lock-held ((state-lock *state*))
                       (gethash channel (state-channels *state*)))))
             (and ch (values (repl:repl-channel-connection ch) (repl:repl-channel-thread ch)))))
          (t (bt2:with-lock-held ((state-lock *state*))
               (let ((name (or (arg-string args "connection") (state-default-connection *state*))))
                 (and name (values (gethash name (state-connections *state*)) nil))))))))

(defun forget-finished-jobs (state)
  "Drop finished jobs past *FINISHED-JOBS-KEPT*. Call with the state lock held."
  (let ((kept 0))
    (setf (state-jobs state)
          (remove-if (lambda (j) (and (not (job-running-p j)) (> (incf kept) *finished-jobs-kept*)))
                     (state-jobs state)))))

;;; The worker

(defun call-transferring-errors (job function)
  "Call FUNCTION in JOB's worker. A REPL-TIMEOUT means the job is still
running: keep waiting. An error is the job's outcome: the TRANSFER-ERROR
restart unwinds to here, and the condition goes to whoever waits for the job
(the worker is a thread boundary, as in lparallel)."
  (restart-case
      (handler-bind ((repl:repl-timeout
                       (lambda (c)
                         (alexandria:when-let ((r (find-restart 'repl:keep-waiting c)))
                           (invoke-restart r))))
                     (error (lambda (c) (invoke-restart 'transfer-error c))))
        (values (funcall function) nil))
    (transfer-error (condition)
      :report "End the job with this error; the caller waiting for it gets it"
      (setf (job-condition job) condition)
      (values nil condition))))

(defun finish-job (job text condition)
  (let ((state *state*))
    (bt2:with-lock-held ((state-lock state))
      (setf (job-result job) (if condition (format nil "Failed: ~A~%" condition) text)
            (job-state job) (if condition :failed :done)
            (job-finished job) (get-internal-real-time))
      (forget-finished-jobs state)
      (bt2:condition-broadcast (state-job-cv state)))
    (when (job-detached job)
      (notify-log (if condition "warning" "notice") "lisp-mcp/jobs"
                  "Job ~A ~:[finished~;failed~] after ~D s: ~A~%~A"
                  (job-id job) condition (job-seconds job) (job-summary job)
                  (first-lines (job-result job) 3)))))

(defun first-lines (text n)
  (let ((lines (uiop:split-string (string-right-trim '(#\Newline) text) :separator '(#\Newline))))
    (format nil "~{~A~^~%~}~:[~;~%… (job-status for the rest)~]"
            (subseq lines 0 (min n (length lines))) (> (length lines) n))))

(defun start-job (tool key args handler)
  "Register a running JOB for TOOL and run HANDLER on ARGS in a worker thread."
  (multiple-value-bind (connection thread) (job-connection-of args)
    (let* ((state *state*)
           (job (bt2:with-lock-held ((state-lock state))
                  (let ((job (make-instance 'job :id (format nil "j~D" (incf (state-job-counter state)))
                                                 :tool tool :key key
                                                 :connection connection :thread thread)))
                    (push job (state-jobs state))
                    job)))
           (server *server*) (verbose *verbose*)
           (lines *max-output-lines*) (chars *max-output-chars*))
      (bt2:make-thread
       (lambda ()
         (let ((*state* state) (*server* server) (*verbose* verbose)
               (*max-output-lines* lines) (*max-output-chars* chars)
               ;; progress belongs to the request, which may be over
               (mcp:*progress-token* nil))
           (multiple-value-bind (result condition)
               (call-transferring-errors job (lambda () (funcall handler args)))
             (finish-job job (if (stringp result) (bound-output tool result) result) condition))))
       :name (format nil "lisp-mcp job ~A ~A" (job-id job) tool))
      job)))

(defun await-job (job seconds)
  "Wait up to SECONDS for JOB. T when it has finished."
  (let ((state *state*)
        (deadline (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (bt2:with-lock-held ((state-lock state))
      (loop while (and (job-running-p job) (< (get-internal-real-time) deadline))
            do (bt2:condition-wait (state-job-cv state) (state-lock state)
                                   :timeout (max 0.01 (/ (- deadline (get-internal-real-time))
                                                         internal-time-units-per-second))))
      (not (job-running-p job)))))

(defun job-outcome (job)
  "JOB's result text; a job that ended with an error signals it again here."
  (if (job-condition job)
      (error (job-condition job))
      (job-result job)))

(defun job-handler (name handler)
  "HANDLER for job tool NAME: run it as a job, wait up to the call's timeout,
then return its result, or the job id while it keeps running. A running job
with the same tool and arguments is not started again."
  (let ((default (cdr (job-tool-p name))))
    (lambda (args)
      (let* ((key (job-arguments-key name args))
             (wait (max 1 (or (arg-int args "timeout") default)))
             (state *state*)
             (running (bt2:with-lock-held ((state-lock state))
                        (let ((j (find-if (lambda (j) (and (job-running-p j) (string= key (job-key j))))
                                          (state-jobs state))))
                          (when j (setf (job-detached j) t))
                          j))))
        (if running
            (running-text running)
            (let ((job (start-job name key args handler)))
              (if (await-job job wait)
                  (progn
                    (bt2:with-lock-held ((state-lock state))
                      (unless (job-detached job)
                        (setf (state-jobs state) (remove job (state-jobs state)))))
                    (job-outcome job))
                  ;; it may finish between the wait and this: then it is just done
                  (if (bt2:with-lock-held ((state-lock state))
                        (or (not (job-running-p job))
                            (progn (setf (job-detached job) t) nil)))
                      (job-outcome job)
                      (running-text job)))))))))

(defun add-job-argument (name schema)
  "SCHEMA with a timeout argument when tool NAME is a job tool."
  (let ((entry (job-tool-p name)))
    (when entry
      (setf (gethash "timeout" (gethash "properties" schema))
            (mcp:json-object "type" "integer"
                             "description" (format nil "Seconds before it continues as a job (default ~D)" (cdr entry))))))
  schema)

;;; Tools

(defun render-job-line (job)
  (format nil "~A  ~(~A~)  ~D s  ~A~%" (job-id job) (job-state job) (job-seconds job) (job-summary job)))

(defun render-job (job)
  (with-text (s)
    (write-string (render-job-line job) s)
    (if (job-running-p job)
        (format s "Still running; job-wait blocks until it is done, job-cancel interrupts it.~%")
        (write-string (job-result job) s))))

(define-tool "job-list" (args :description "Background jobs (long repl-* calls): id, state, seconds, tool and arguments.")
    (input-schema)
  (let ((jobs (bt2:with-lock-held ((state-lock *state*)) (copy-list (state-jobs *state*)))))
    (if jobs
        (format nil "~{~A~}" (mapcar #'render-job-line jobs))
        (format nil "No jobs~%"))))

(define-tool "job-status" (args :description "State of a background job, with its result once it is done.")
    (input-schema '("id" "string" "Job id (jN)" :required t))
  (render-job (find-job (require-arg args "id"))))

(define-tool "job-wait" (args :description "Wait (bounded) for a background job to finish; returns its status and result.")
    (input-schema '("id" "string" "Job id (jN)" :required t)
                  '("timeout" "integer" "Seconds to wait (default 60, at most 300)"))
  (let ((job (find-job (require-arg args "id"))))
    (await-job job (min *job-wait-limit* (max 1 (arg-int args "timeout" 60))))
    (render-job job)))

(define-tool "job-cancel" (args :description "Interrupt a running job's evaluation (repl-interrupt); it then reports the debugger state."
                                :kind :write)
    (input-schema '("id" "string" "Job id (jN)" :required t))
  (let ((job (find-job (require-arg args "id"))))
    (cond ((not (job-running-p job))
           (format nil "Job ~A is not running (~(~A~))~%" (job-id job) (job-state job)))
          ((null (job-connection job))
           (error 'bad-argument :message (format nil "Job ~A has no connection to interrupt" (job-id job))))
          (t (repl:repl-interrupt (job-connection job) :thread (job-interrupt-thread job))
             (format nil "Interrupt sent to job ~A; it reports the debugger state (job-status / job-wait).~%"
                     (job-id job))))))
