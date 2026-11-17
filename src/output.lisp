(in-package #:lisp-mcp)

;;; Bounded tool output. Every tool result goes through BOUND-OUTPUT: text
;;; over *MAX-OUTPUT-LINES* lines or *MAX-OUTPUT-CHARS* characters comes back
;;; as its head and tail plus a note, and the whole text is kept in an output
;;; store. output-read and the lisp-output:// resources read it back a page or
;;; a search at a time, so a long build log, test run or printed value costs
;;; tokens only for the part that is asked for.

(defvar *max-output-lines* 200
  "Lines a tool result may have before it is cut and stored.")
(defvar *max-output-chars* 16000
  "Characters a tool result may have before it is cut and stored.")
(defvar *max-line-chars* 2000
  "Longer lines are cut in returned text; the store keeps them whole.")
(defvar *output-store-size* 64
  "Stored outputs kept; older ones are dropped.")
(defparameter *unbounded-tools* '("output-read")
  "Tools whose results are never stored: they page by themselves.")

(defun env-integer (name)
  (let ((v (uiop:getenv name)))
    (and v (parse-integer v :junk-allowed t))))

(defun configure-output-limits (&key lines chars store)
  "Set the output limits from LINES / CHARS / STORE (integers or numeric
strings), else from LISP_MCP_MAX_OUTPUT_LINES, LISP_MCP_MAX_OUTPUT_CHARS and
LISP_MCP_OUTPUT_STORE_SIZE. Read at server start, not at build time."
  (flet ((pick (value env current)
           (let ((v (cond ((integerp value) value)
                          ((stringp value) (parse-integer value :junk-allowed t))
                          (t (env-integer env)))))
             (if (and v (plusp v)) v current))))
    (setf *max-output-lines* (pick lines "LISP_MCP_MAX_OUTPUT_LINES" *max-output-lines*)
          *max-output-chars* (pick chars "LISP_MCP_MAX_OUTPUT_CHARS" *max-output-chars*)
          *output-store-size* (pick store "LISP_MCP_OUTPUT_STORE_SIZE" *output-store-size*))))

(defclass stored-output ()
  ((id :initarg :id :reader output-id)
   (tool :initarg :tool :reader output-tool)
   (lines :initarg :lines :reader output-lines :type simple-vector)
   (chars :initarg :chars :reader output-chars)))

(defun text-lines (text)
  "TEXT as a vector of lines; a final newline does not add an empty line."
  (let ((lines (uiop:split-string text :separator '(#\Newline))))
    (when (and (rest lines) (string= "" (car (last lines))))
      (setf lines (butlast lines)))
    (coerce lines 'simple-vector)))

(defun output-text (entry)
  (format nil "~{~A~%~}" (coerce (output-lines entry) 'list)))

(defun store-output (tool text lines &key (state *state*))
  "Keep LINES (of TEXT, from TOOL) under a fresh id; drop the oldest past
*OUTPUT-STORE-SIZE*."
  (bt2:with-lock-held ((state-lock state))
    (let ((entry (make-instance 'stored-output
                                :id (format nil "o~D" (incf (state-output-counter state)))
                                :tool tool :lines lines :chars (length text))))
      (push entry (state-outputs state))
      (let ((tail (nthcdr (1- (max 1 *output-store-size*)) (state-outputs state))))
        (when tail (setf (cdr tail) nil)))
      entry)))

(defun find-output (id &key (state *state*))
  (bt2:with-lock-held ((state-lock state))
    (or (find (string-trim " " id) (state-outputs state) :key #'output-id :test #'string-equal)
        (error 'bad-argument
               :message (format nil "No stored output ~S. Kept (newest first, at most ~D): ~:[none~;~:*~{~A~^, ~}~]"
                                id *output-store-size* (mapcar #'output-id (state-outputs state)))))))

(defun clip-line (line)
  (if (<= (length line) *max-line-chars*)
      line
      (format nil "~A… [+~D chars]" (subseq line 0 *max-line-chars*) (- (length line) *max-line-chars*))))

(defun clip-around (line position)
  "LINE cut to *MAX-LINE-CHARS* around POSITION (a match), with … where cut."
  (if (<= (length line) *max-line-chars*)
      line
      (let* ((start (max 0 (min (- position (floor *max-line-chars* 2))
                                (- (length line) *max-line-chars*))))
             (end (min (length line) (+ start *max-line-chars*))))
        (format nil "~:[~;…~]~A~:[~;…~]" (plusp start) (subseq line start end) (< end (length line))))))

(defun output-fits-p (text lines)
  (and (<= (length lines) *max-output-lines*)
       (<= (length text) *max-output-chars*)
       (every (lambda (l) (<= (length l) *max-line-chars*)) lines)))

(defun take-lines (lines indices budget)
  "The prefix of INDICES whose clipped lines fit in BUDGET characters."
  (loop for i in indices
        for size = (1+ (length (clip-line (svref lines i))))
        while (>= budget size)
        do (decf budget size)
        collect i))

(defun bound-output (tool text)
  "TEXT if it fits the limits. Otherwise store it and return its head (3/5 of
the budget) and tail (2/5: errors and summaries come last) around a note
naming the stored id and how to read the rest."
  (let ((lines (text-lines text)))
    (if (output-fits-p text lines)
        text
        (let* ((n (length lines))
               (entry (store-output tool text lines))
               (id (output-id entry))
               (head (take-lines lines (alexandria:iota (min n (ceiling (* 3 *max-output-lines*) 5)))
                                 (floor (* 3 *max-output-chars*) 5)))
               (after (length head))
               (tail (reverse (take-lines lines
                                          (loop for i from (1- n) downto after
                                                repeat (- *max-output-lines* after)
                                                collect i)
                                          (floor (* 2 *max-output-chars*) 5))))
               (gap-end (if tail (first tail) n)))
          (with-text (s)
            (dolist (i head) (write-line (clip-line (svref lines i)) s))
            (if (< after gap-end)
                (format s "[… lines ~D-~D of ~D not shown (~D chars in all). Stored as ~A: output-read id=~A offset=~D pages on, grep=\"text\" searches; resource lisp-output://~A]~%"
                        (1+ after) gap-end n (length text) id id (1+ after) id)
                (format s "[long lines cut. Full text stored as ~A: output-read id=~A; resource lisp-output://~A]~%"
                        id id id))
            (dolist (i tail) (write-line (clip-line (svref lines i)) s)))))))

(defun bounded-handler (name handler)
  "HANDLER, with string results passed through BOUND-OUTPUT."
  (if (member name *unbounded-tools* :test #'string=)
      handler
      (lambda (args)
        (let ((result (funcall handler args)))
          (if (stringp result) (bound-output name result) result)))))

;;; Reading stored output

(defun render-output-list (&key (state *state*))
  (let ((outputs (bt2:with-lock-held ((state-lock state)) (copy-list (state-outputs state)))))
    (if (null outputs)
        (format nil "No stored outputs~%")
        (with-text (s)
          (dolist (o outputs)
            (format s "~A  ~A  ~D lines, ~D chars~%"
                    (output-id o) (output-tool o) (length (output-lines o)) (output-chars o)))))))

(defun render-output-page (entry offset limit)
  "Lines OFFSET (1-based) on, at most LIMIT and *MAX-OUTPUT-CHARS*, numbered."
  (let* ((lines (output-lines entry))
         (n (length lines))
         (start (max 0 (1- offset))))
    (if (>= start n)
        (format nil "~A has ~D line~:P; offset ~D is past the end~%" (output-id entry) n offset)
        (let* ((shown (take-lines lines
                                  (loop for i from start below (min n (+ start (max 1 limit))) collect i)
                                  *max-output-chars*))
               (end (+ start (max 1 (length shown)))))
          (with-text (s)
            (format s "~A (~A): lines ~D-~D of ~D~%" (output-id entry) (output-tool entry) (1+ start) end n)
            (loop for i from start below end
                  do (format s "~6D: ~A~%" (1+ i) (clip-line (svref lines i))))
            (when (< end n)
              (format s "[continue with offset=~D]~%" (1+ end))))))))

(defun grep-terms (pattern)
  (remove "" (mapcar (lambda (p) (string-trim " " p)) (uiop:split-string pattern :separator "|"))
          :test #'string=))

(defun line-match (line terms)
  "Position of the first of TERMS (case-insensitive) in LINE, or NIL."
  (loop for term in terms
        for p = (search term line :test #'char-equal)
        when p return p))

(defun render-output-grep (entry pattern context)
  "Lines of ENTRY matching PATTERN (|-separated alternatives), grep -n style:
matches marked :, CONTEXT lines around them marked -, groups split by --."
  (let* ((lines (output-lines entry))
         (n (length lines))
         (terms (or (grep-terms pattern)
                    (error 'bad-argument :message "grep needs some text")))
         (hits (loop for i below n when (line-match (svref lines i) terms) collect i))
         (budget *max-output-chars*)
         (last -1))
    (with-text (s)
      (format s "~A (~A): ~D matching line~:P of ~D for ~S~%"
              (output-id entry) (output-tool entry) (length hits) n pattern)
      (loop for (h . more) on hits
            do (let ((from (max (1+ last) (- h context)))
                     (to (min (1- n) (+ h context))))
                 (when (and (>= last 0) (> from (1+ last))) (write-line "--" s))
                 (loop for i from from to to
                       for line = (svref lines i)
                       for p = (line-match line terms)
                       for text = (format nil "~6D~:[-~;:~] ~A" (1+ i) p (if p (clip-around line p) (clip-line line)))
                       do (decf budget (1+ (length text)))
                          (write-line text s)
                          (setf last i))
                 (when (and more (minusp budget))
                   (format s "[~D more match~:*~[es~;~:;es~] not shown: narrow grep, or page with offset]~%"
                           (length more))
                   (return)))))))

(define-tool "output-read"
    (args :description "Read a tool result that was cut short; its note names the id (e.g. o3). Page with offset/limit, or search with grep (only matching lines, with context). No id lists stored outputs.")
    (input-schema '("id" "string" "Stored output id, from a cut-short result")
                  '("offset" "integer" "First line, 1-based (default 1)")
                  '("limit" "integer" "Lines to return (default 100)")
                  '("grep" "string" "Only lines containing this text (case-insensitive; | separates alternatives)")
                  '("context" "integer" "Lines shown around each grep match (default 2)"))
  (let ((id (arg-string args "id")))
    (if (null id)
        (render-output-list)
        (let ((entry (find-output id))
              (pattern (arg-string args "grep")))
          (if pattern
              (render-output-grep entry pattern (max 0 (arg-int args "context" 2)))
              (render-output-page entry (arg-int args "offset" 1) (arg-int args "limit" 100)))))))

(defun register-output-resources (server)
  "lisp-output://{id} (whole text), …/lines/{start}-{end} and …/grep/{+pattern}."
  (flet ((ids (name value)
           (declare (ignore name))
           (complete-prefix (mapcar #'output-id (state-outputs *state*)) value))
         (var (vars name) (cdr (assoc name vars :test #'string=))))
    (mcp:register-resource-template
     server
     (mcp:make-mcp-resource-template
      "lisp-output://{id}" :name "lisp-output" :mime-type "text/plain"
      :description "Whole text of a tool result that was cut short (id from its note)."
      :complete #'ids
      :handler (lambda (uri vars)
                 (declare (ignore uri))
                 (resource-text (lambda () (output-text (find-output (var vars "id"))))))))
    (mcp:register-resource-template
     server
     (mcp:make-mcp-resource-template
      "lisp-output://{id}/lines/{start}-{end}" :name "lisp-output-lines" :mime-type "text/plain"
      :description "Lines START to END (1-based, inclusive) of a stored tool result, numbered."
      :complete #'ids
      :handler (lambda (uri vars)
                 (declare (ignore uri))
                 (resource-text
                  (lambda ()
                    (let ((start (parse-integer (var vars "start") :junk-allowed t))
                          (end (parse-integer (var vars "end") :junk-allowed t)))
                      (unless (and start end (<= 1 start end))
                        (error 'bad-argument :message "lines/{start}-{end} needs 1 <= start <= end"))
                      (render-output-page (find-output (var vars "id")) start (1+ (- end start)))))))))
    (mcp:register-resource-template
     server
     (mcp:make-mcp-resource-template
      "lisp-output://{id}/grep/{+pattern}" :name "lisp-output-grep" :mime-type "text/plain"
      :description "Lines of a stored tool result containing PATTERN (| separates alternatives), with 2 lines of context."
      :complete #'ids
      :handler (lambda (uri vars)
                 (declare (ignore uri))
                 (resource-text
                  (lambda () (render-output-grep (find-output (var vars "id")) (var vars "pattern") 2))))))))
