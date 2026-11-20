(in-package #:lisp-mcp)

;;; FORM-RESOURCES (H4a, pure): a conservative scanner from Lisp code to
;;; permission resources. It reads with lisp-source-protocol (Eclector CST:
;;; never evaluates, never interns; #. becomes a placeholder) and walks the
;;; forms without macroexpansion: special forms and a known list of standard
;;; macros and functions are modelled, any other operator is named as
;;; "lisp:calls <pkg>:<name>", and what cannot be attributed to a name is
;;; "lisp:*". The grammar and the tables are in the H4a brief
;;; (cl-stack docs/capabilities/lisp-agent-tools.md).
;;;
;;; The walker matches on SRC:NODE-VALUE of each top-level form: a cons tree
;;; of SRC:SYMBOL-TOKENs (quote, function and the quasiquote markers come as
;;; real symbols). Operators are looked up by their canonical "<pkg>:<name>"
;;; string, so nothing is interned; the only symbol lookup is FIND-SYMBOL in
;;; COMMON-LISP, to tell whether an unqualified name is a CL external.
;;; A CST pre-pass finds what the values lose: #. and #S (lisp:read-eval)
;;; and #n= / #n# labels (which can make cycles: the result is then
;;; ("lisp:*")). Every user cons is traversed through %ELEMENTS (bounded,
;;; dotted lists allowed) and nesting is bounded by *SCAN-MAX-DEPTH*, so the
;;; walk is total.

(defparameter *scan-max-depth* 200
  "Nesting depth past which the scanner stops descending and emits lisp:*.")

(defvar *scan-depth* 0 "Current nesting depth of the walk.")

(defparameter +scan-max-elements+ 100000
  "Most list elements %ELEMENTS takes from one cons chain.")

;;; Collector

(defstruct (scan (:constructor %make-scan (package)))
  (package "cl-user" :type string)
  (out '())
  (star nil))

(defun %emit (scan resource)
  (if (string= resource "lisp:*")
      (setf (scan-star scan) t)
      (pushnew resource (scan-out scan) :test #'string=))
  nil)

(defun %star (scan) (%emit scan "lisp:*"))

(defun %finish (scan form-count)
  "Ordered resources without duplicates, lisp:* last, lisp:pure only alone."
  (let ((out (reverse (scan-out scan))))
    (cond ((scan-star scan) (append out (list "lisp:*")))
          (out out)
          ((plusp form-count) (list "lisp:pure"))
          (t nil))))

;;; Lexical environment (functional): VARS is an alist from canonical names
;;; to :LEXICAL, :FRESH, :DYNAMIC or (:ALIAS form . env) for with-slots /
;;; with-accessors; FNS lists the canonical names of flet / labels functions.

(defstruct (scan-env (:constructor %make-env (&optional vars fns)) (:copier nil))
  (vars '())
  (fns '()))

(defun %env-var (env key)
  (cdr (assoc key (scan-env-vars env) :test #'string=)))

(defun %env-fn-p (env key)
  (and (stringp key) (member key (scan-env-fns env) :test #'string=) t))

(defun %env-bind (env key kind)
  (%make-env (acons key kind (scan-env-vars env)) (scan-env-fns env)))

(defun %env-bind-fns (env keys)
  (%make-env (scan-env-vars env) (append keys (scan-env-fns env))))

;;; Tokens and canonical names

(defun %token-p (x) (src:symbol-token-p x))

(defun %canonical-name (name)
  (if (some #'lower-case-p name)
      (concatenate 'string "|" name "|")
      (string-downcase name)))

(defun %canonical-package (name)
  "Canonical package name: cl, cl-user, keyword, else NAME downcased."
  (let ((s (string-left-trim ":#" (string name))))
    (cond ((member s '("CL" "COMMON-LISP") :test #'string-equal) "cl")
          ((member s '("CL-USER" "COMMON-LISP-USER") :test #'string-equal) "cl-user")
          ((string-equal s "KEYWORD") "keyword")
          (t (string-downcase s)))))

(defun %cl-external-p (name)
  (eq :external (nth-value 1 (find-symbol name "COMMON-LISP"))))

(defun %sym (token scan)
  "TOKEN as \"<pkg>:<name>\" (see the brief's <sym>)."
  (let ((pkg (src:symbol-token-package token))
        (name (src:symbol-token-name token)))
    (cond ((null pkg) (concatenate 'string "#:" (%canonical-name name)))
          ((eq pkg :keyword) (concatenate 'string "keyword:" (%canonical-name name)))
          ((eq pkg :current)
           (concatenate 'string (if (%cl-external-p name) "cl" (scan-package scan))
                        ":" (%canonical-name name)))
          (t (concatenate 'string (%canonical-package pkg) ":" (%canonical-name name))))))

(defun %token-named-p (x &rest names)
  "X is a token whose name (any package) is one of NAMES."
  (and (%token-p x) (member (src:symbol-token-name x) names :test #'string-equal) t))

(defun %keyword-token-p (x)
  (and (%token-p x) (eq (src:symbol-token-package x) :keyword)))

(defun %constant-token-p (x)
  "NIL, T or a keyword."
  (and (%token-p x)
       (or (eq (src:symbol-token-package x) :keyword)
           (and (member (src:symbol-token-package x) '(:current "CL" "COMMON-LISP") :test #'equal)
                (member (src:symbol-token-name x) '("NIL" "T") :test #'string=)))))

(defun %nil-p (x)
  (or (null x)
      (and (%constant-token-p x) (string= (src:symbol-token-name x) "NIL")
           (not (eq (src:symbol-token-package x) :keyword)))))

(defun %earmuffed-p (token)
  (let ((n (src:symbol-token-name token)))
    (and (> (length n) 2) (char= (char n 0) #\*) (char= (char n (1- (length n))) #\*))))

(defun %marker (x)
  "The quasiquote marker X stands for (:QQ :UQ :UQS), or NIL."
  (when (and (symbolp x) x (symbol-package x)
             (string= (package-name (symbol-package x)) "ECLECTOR.READER"))
    (let ((n (symbol-name x)))
      (cond ((string= n "QUASIQUOTE") :qq)
            ((string= n "UNQUOTE") :uq)
            ((string= n "UNQUOTE-SPLICING") :uqs)))))

(defun %op-key (x scan)
  "Canonical operator string of X, a quasiquote marker keyword, or NIL."
  (cond ((%token-p x) (%sym x scan))
        ((eq x 'quote) "cl:quote")
        ((eq x 'function) "cl:function")
        (t (%marker x))))

(defun %elements (x)
  "→ (values items proper-p tail): the elements of the cons chain X, bounded;
TAIL is the non-NIL atom ending a dotted list."
  (let ((items '()) (tail x))
    (loop for i below +scan-max-elements+
          while (consp tail)
          do (push (car tail) items)
             (setf tail (cdr tail)))
    (values (nreverse items) (null tail) (if (consp tail) nil tail))))

(defun %head-key (form scan)
  (and (consp form) (%op-key (car form) scan)))

(defun %quoted-token (x scan)
  "The token of 'TOKEN (quote form), else NIL."
  (when (and (consp x) (equal (%head-key x scan) "cl:quote"))
    (let ((it (second (%elements x))))
      (and (%token-p it) it))))

(defun %function-name (x scan)
  "Canonical name of a function name X (a token or (setf token)), else NIL."
  (cond ((%token-p x) (%sym x scan))
        ((and (consp x) (%token-named-p (car x) "SETF"))
         (let ((n (second (%elements x))))
           (and (%token-p n) (format nil "(setf ~A)" (%sym n scan)))))))

;;; Tables

(defvar *scan-operators* (make-hash-table :test #'equal)
  "Canonical operator name → (lambda (args env scan)).")

(defvar *scan-fn-kinds* (make-hash-table :test #'equal)
  "Canonical function name → :PURE, :IO or :READ-EVAL, for #'name.")

(defvar *scan-constructors* (make-hash-table :test #'equal)
  "Functions whose result is a fresh object.")

(defvar *scan-accessors* (make-hash-table :test #'equal)
  "Setf-able CL accessors → index of the argument the place is rooted in.")

(defun %op-name (name)
  (if (find #\: name) name (concatenate 'string "cl:" name)))

(defmacro define-scan-operator (names (args env scan) &body body)
  "Model the operators NAMES (CL names unless qualified)."
  `(let ((fn (lambda (,args ,env ,scan) (declare (ignorable ,args ,env ,scan)) ,@body)))
     (dolist (n ',names) (setf (gethash (%op-name n) *scan-operators*) fn))))

(defparameter +scan-pure-functions+
  '(;; numbers
    "+" "-" "*" "/" "1+" "1-" "=" "/=" "<" ">" "<=" ">=" "min" "max" "abs" "mod" "rem"
    "floor" "ceiling" "truncate" "round" "ffloor" "fceiling" "ftruncate" "fround" "gcd" "lcm"
    "expt" "exp" "log" "sqrt" "isqrt" "sin" "cos" "tan" "asin" "acos" "atan" "sinh" "cosh" "tanh"
    "signum" "numerator" "denominator" "realpart" "imagpart" "complex" "conjugate" "phase" "cis"
    "float" "rational" "rationalize" "zerop" "plusp" "minusp" "oddp" "evenp" "numberp" "integerp"
    "rationalp" "floatp" "realp" "complexp" "ash" "logand" "logior" "logxor" "lognot" "logeqv"
    "lognand" "lognor" "logandc1" "logandc2" "logorc1" "logorc2" "logbitp" "logcount" "logtest"
    "integer-length" "byte" "byte-size" "byte-position" "ldb" "dpb" "ldb-test" "mask-field"
    "deposit-field" "parse-integer" "decode-float" "scale-float" "float-sign" "float-digits"
    "float-precision" "integer-decode-float" "boole"
    ;; equality, logic, functions
    "eq" "eql" "equal" "equalp" "not" "null" "identity" "values" "values-list" "constantly"
    "complement"
    ;; conses and lists
    "car" "cdr" "caar" "cadr" "cdar" "cddr" "caddr" "cdddr" "cadddr" "cddddr" "first" "second"
    "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth" "tenth" "rest" "nth" "nthcdr"
    "last" "butlast" "list" "list*" "cons" "make-list" "append" "copy-list" "copy-tree"
    "copy-alist" "list-length" "length" "endp" "consp" "listp" "atom" "member" "member-if"
    "member-if-not" "assoc" "assoc-if" "assoc-if-not" "rassoc" "rassoc-if" "rassoc-if-not"
    "acons" "pairlis" "getf" "get-properties" "tailp" "ldiff" "subst" "subst-if" "subst-if-not"
    "sublis" "adjoin" "union" "intersection" "set-difference" "set-exclusive-or" "subsetp"
    "revappend" "tree-equal"
    ;; sequences and arrays
    "elt" "subseq" "copy-seq" "reverse" "concatenate" "count" "count-if" "count-if-not" "find"
    "find-if" "find-if-not" "position" "position-if" "position-if-not" "remove" "remove-if"
    "remove-if-not" "remove-duplicates" "substitute" "substitute-if" "substitute-if-not" "search"
    "mismatch" "every" "some" "notany" "notevery" "reduce" "map" "mapcar" "mapc" "mapcan"
    "maplist" "mapl" "mapcon" "make-sequence" "vector" "make-array" "aref" "svref"
    "array-dimension" "array-dimensions" "array-rank" "array-total-size" "array-element-type"
    "adjustable-array-p" "array-has-fill-pointer-p" "fill-pointer" "row-major-aref" "vectorp"
    "simple-vector-p" "bit" "sbit" "arrayp" "sequencep"
    ;; characters and strings
    "char" "schar" "string" "string=" "string/=" "string<" "string>" "string<=" "string>="
    "string-equal" "string-not-equal" "string-lessp" "string-greaterp" "string-not-lessp"
    "string-not-greaterp" "string-upcase" "string-downcase" "string-capitalize" "string-trim"
    "string-left-trim" "string-right-trim" "make-string" "char=" "char/=" "char<" "char>" "char<="
    "char>=" "char-equal" "char-not-equal" "char-lessp" "char-greaterp" "char-upcase"
    "char-downcase" "char-code" "code-char" "char-int" "char-name" "name-char" "character"
    "characterp" "alpha-char-p" "digit-char-p" "alphanumericp" "upper-case-p" "lower-case-p"
    "both-case-p" "graphic-char-p" "standard-char-p" "digit-char" "stringp"
    ;; hash tables (reads)
    "gethash" "make-hash-table" "hash-table-count" "hash-table-p" "hash-table-test"
    "hash-table-size" "sxhash"
    ;; types and classes
    "type-of" "typep" "subtypep" "class-of" "find-class" "class-name" "symbolp" "keywordp"
    "functionp" "packagep" "streamp" "pathnamep" "slot-value" "slot-boundp"
    ;; symbols and packages (no interning)
    "symbol-name" "symbol-package" "find-symbol" "find-package" "package-name"
    "package-nicknames" "boundp" "fboundp"
    ;; printing to strings and string streams
    "princ-to-string" "prin1-to-string" "write-to-string" "make-string-output-stream"
    "make-string-input-stream")
  "CL data functions the scanner treats as pure.")

(defparameter +scan-constructor-functions+
  '("list" "list*" "cons" "vector" "make-list" "make-array" "make-string" "make-hash-table"
    "copy-list" "copy-seq" "copy-tree" "copy-alist" "mapcar" "subseq" "reverse" "concatenate"
    "make-string-output-stream" "make-string-input-stream"))

(defparameter +scan-accessor-roots+
  '(("car" . 0) ("cdr" . 0) ("caar" . 0) ("cadr" . 0) ("cdar" . 0) ("cddr" . 0) ("caddr" . 0)
    ("cdddr" . 0) ("first" . 0) ("second" . 0) ("third" . 0) ("fourth" . 0) ("fifth" . 0)
    ("sixth" . 0) ("seventh" . 0) ("eighth" . 0) ("ninth" . 0) ("tenth" . 0) ("rest" . 0)
    ("nth" . 1) ("elt" . 0) ("aref" . 0) ("svref" . 0) ("char" . 0) ("schar" . 0) ("bit" . 0)
    ("sbit" . 0) ("row-major-aref" . 0) ("gethash" . 1) ("slot-value" . 0) ("getf" . 0)
    ("get" . 0) ("fill-pointer" . 0) ("subseq" . 0) ("symbol-plist" . 0))
  "Setf-able accessors and the index of the argument their place is rooted in.")

(defparameter +scan-function-arguments+
  '(("funcall" 0) ("apply" 0) ("mapcar" 0) ("mapc" 0) ("mapcan" 0) ("maplist" 0) ("mapl" 0)
    ("mapcon" 0) ("map" 1) ("map-into" 1) ("reduce" 0) ("every" 0) ("some" 0) ("notany" 0)
    ("notevery" 0) ("count-if" 0) ("count-if-not" 0) ("find-if" 0) ("find-if-not" 0)
    ("position-if" 0) ("position-if-not" 0) ("remove-if" 0) ("remove-if-not" 0) ("member-if" 0)
    ("member-if-not" 0) ("assoc-if" 0) ("assoc-if-not" 0) ("rassoc-if" 0) ("rassoc-if-not" 0)
    ("substitute-if" 1) ("substitute-if-not" 1) ("subst-if" 1) ("subst-if-not" 1)
    ("delete-if" 0) ("delete-if-not" 0) ("nsubstitute-if" 1) ("nsubstitute-if-not" 1)
    ("sort" 1) ("stable-sort" 1) ("merge" 3) ("complement" 0))
  "Functions with function-designator arguments, and their indices.")

(defparameter +scan-key-functions+
  '("member" "assoc" "rassoc" "find" "position" "count" "remove" "remove-duplicates" "delete"
    "delete-duplicates" "substitute" "nsubstitute" "search" "mismatch" "sort" "stable-sort"
    "merge" "reduce" "adjoin" "union" "intersection" "set-difference" "set-exclusive-or"
    "subsetp" "nunion" "nintersection" "nset-difference" "sublis" "subst" "nsubst" "tree-equal"
    "count-if" "count-if-not" "find-if" "find-if-not" "position-if" "position-if-not"
    "remove-if" "remove-if-not" "member-if" "member-if-not" "assoc-if" "assoc-if-not"
    "rassoc-if" "rassoc-if-not" "delete-if" "delete-if-not" "substitute-if" "substitute-if-not"
    "nsubstitute-if" "nsubstitute-if-not" "subst-if" "subst-if-not")
  "Functions whose :key / :test / :test-not arguments are function designators.")

(defparameter +scan-destructive-functions+
  '(("sort" 0) ("stable-sort" 0) ("nreverse" 0) ("nconc" :all-but-last) ("delete" 1)
    ("delete-if" 1) ("delete-if-not" 1) ("delete-duplicates" 0) ("nsubst" 2) ("nsubst-if" 2)
    ("nsubst-if-not" 2) ("nsubstitute" 2) ("nsubstitute-if" 2) ("nsubstitute-if-not" 2)
    ("fill" 0) ("replace" 0) ("map-into" 0) ("rplaca" 0) ("rplacd" 0) ("remhash" 1)
    ("clrhash" 0) ("vector-push" 1) ("vector-push-extend" 1) ("vector-pop" 0)
    ("adjust-array" 0) ("nunion" 0 1) ("nintersection" 0) ("nset-difference" 0)
    ("nset-exclusive-or" 0 1) ("nbutlast" 0) ("nreconc" 0) ("nstring-upcase" 0)
    ("nstring-downcase" 0) ("nstring-capitalize" 0) ("get-output-stream-string" 0))
  "Destructive functions: the indices of the arguments they modify.")

(defparameter +scan-stream-functions+
  '(("print" . 1) ("prin1" . 1) ("princ" . 1) ("pprint" . 1) ("write-string" . 1)
    ("write-line" . 1) ("write-char" . 1) ("write-byte" . 1) ("write-sequence" . 1)
    ("terpri" . 0) ("fresh-line" . 0) ("finish-output" . 0) ("force-output" . 0)
    ("clear-output" . 0) ("read-line" . 0) ("read-char" . 0) ("read-char-no-hang" . 0)
    ("read-byte" . 0) ("peek-char" . 1) ("unread-char" . 1) ("listen" . 0) ("clear-input" . 0)
    ("read-sequence" . 1))
  "Stream functions and the index of their stream argument: I/O unless it is a
fresh string stream.")

(defparameter +scan-io-functions+
  '("open" "probe-file" "directory" "delete-file" "rename-file" "ensure-directories-exist"
    "load" "compile-file" "describe" "truename" "file-write-date" "file-author" "file-length"
    "y-or-n-p" "yes-or-no-p" "dribble" "ed" "inspect" "room" "require" "provide"
    "uiop:run-program" "uiop:launch-program" "uiop:read-file-string" "uiop:read-file-lines"
    "uiop:read-file-form" "uiop:read-file-forms" "uiop:delete-file-if-exists"
    "uiop:delete-directory-tree" "uiop:delete-empty-directory" "uiop:rename-file-overwriting-target"
    "uiop:ensure-all-directories-exist" "uiop:copy-file" "uiop:directory-files"
    "uiop:subdirectories" "uiop:file-exists-p" "uiop:directory-exists-p" "uiop:probe-file*")
  "Functions that touch files, processes or the terminal.")

(defparameter +scan-read-functions+
  '("read" "read-preserving-whitespace" "read-from-string" "read-delimited-list")
  "Readers: with *read-eval* at its default they can run #.")

(defparameter +scan-package-functions+
  '(("make-package" . 0) ("delete-package" . 0) ("rename-package" . 0) ("use-package" . 1)
    ("unuse-package" . 1) ("export" . 1) ("unexport" . 1) ("import" . 1) ("shadow" . 1)
    ("shadowing-import" . 1) ("intern" . 1) ("unintern" . 1))
  "Package operators and the index of their package argument.")

(defparameter +scan-loop-keywords+
  '("FOR" "AS" "WITH" "INTO" "NAMED" "USING" "IN" "ON" "FROM" "TO" "BELOW" "ABOVE" "BY"
    "DOWNFROM" "DOWNTO" "UPFROM" "UPTO" "ACROSS" "BEING" "THE" "EACH" "HASH-KEY" "HASH-KEYS"
    "HASH-VALUE" "HASH-VALUES" "OF" "SYMBOL" "SYMBOLS" "PRESENT-SYMBOL" "PRESENT-SYMBOLS"
    "EXTERNAL-SYMBOL" "EXTERNAL-SYMBOLS" "=" "THEN" "AND" "COLLECT" "COLLECTING" "APPEND"
    "APPENDING" "NCONC" "NCONCING" "SUM" "SUMMING" "COUNT" "COUNTING" "MAXIMIZE" "MAXIMIZING"
    "MINIMIZE" "MINIMIZING" "DO" "DOING" "RETURN" "WHEN" "IF" "UNLESS" "ELSE" "END" "WHILE"
    "UNTIL" "ALWAYS" "NEVER" "THEREIS" "REPEAT" "INITIALLY" "FINALLY" "IT" "OF-TYPE")
  "LOOP keywords (matched by name in any package): not forms.")

(defparameter +scan-loop-binders+ '("FOR" "AS" "WITH" "INTO" "USING")
  "LOOP keywords followed by a binding pattern.")

;;; The walker

(defun %walk (form env scan)
  (cond ((%token-p form) (%walk-var form env scan))
        ((consp form)
         (let ((*scan-depth* (1+ *scan-depth*)))
           (if (> *scan-depth* *scan-max-depth*)
               (%star scan)
               (%walk-call form env scan))))
        (t nil)))

(defun %walk-forms (forms env scan)
  (dolist (f forms) (%walk f env scan)))

(defun %walk-var (token env scan)
  (unless (%constant-token-p token)
    (let ((key (%sym token scan)))
      (when (and (null (%env-var env key)) (%earmuffed-p token))
        (%emit scan (concatenate 'string "lisp:reads " key))))))

(defun %lambda-form-p (x scan)
  (and (consp x) (equal (%head-key x scan) "cl:lambda")))

(defun %walk-call (form env scan)
  (multiple-value-bind (items proper) (%elements form)
    (unless proper (%star scan))
    (let* ((op (first items))
           (args (rest items))
           (key (%op-key op scan)))
      (cond ((%lambda-form-p op scan)
             (%walk-lambda (rest (%elements op)) env scan)
             (%walk-forms args env scan))
            ((eq key :qq) (%walk-qq (first args) 1 env scan))
            ((null key) (%star scan) (%walk-forms args env scan))
            ((keywordp key) (%star scan) (%walk-forms args env scan))
            ((%env-fn-p env key) (%walk-forms args env scan))
            (t (let ((handler (gethash key *scan-operators*)))
                 (if handler
                     (funcall handler args env scan)
                     (progn (%emit scan (concatenate 'string "lisp:calls " key))
                            (%walk-forms args env scan)))))))))

(defun %walk-qq (x level env scan)
  "Quasiquoted X: only the unquoted parts (at LEVEL 1) are walked as forms."
  (let ((*scan-depth* (1+ *scan-depth*)))
    (cond ((> *scan-depth* *scan-max-depth*) (%star scan))
          ((consp x)
           (let ((key (%op-key (car x) scan)))
             (case key
               ((:uq :uqs)
                (let ((inner (second (%elements x))))
                  (if (<= level 1)
                      (%walk inner env scan)
                      (%walk-qq inner (1- level) env scan))))
               (:qq (%walk-qq (second (%elements x)) (1+ level) env scan))
               (t (loop for tail = x then (cdr tail)
                        for i from 0 below +scan-max-elements+
                        while (consp tail)
                        do (if (and (plusp i) (member (%marker (car tail)) '(:uq :uqs)))
                               ;; `(a . ,b) reads as (a unquote b)
                               (progn (%walk-qq tail level env scan) (return))
                               (%walk-qq (car tail) level env scan))
                        finally (when tail (%walk-qq tail level env scan)))))))
          ((and (simple-vector-p x))
           (loop for e across x do (%walk-qq e level env scan)))
          (t nil))))

;;; Function designators

(defun %emit-function-head (key scan)
  "The effect of calling the function named KEY with arguments not seen."
  (case (gethash key *scan-fn-kinds*)
    (:pure nil)
    (:io (%emit scan "lisp:io"))
    (:read-eval (%emit scan "lisp:read-eval"))
    (t (if (gethash key *scan-operators*)
           (%star scan)
           (%emit scan (concatenate 'string "lisp:calls " key))))))

(defun %walk-function (x env scan)
  "The argument of (function X)."
  (cond ((%token-p x)
         (let ((key (%sym x scan)))
           (unless (%env-fn-p env key) (%emit-function-head key scan))))
        ((%lambda-form-p x scan) (%walk-lambda (rest (%elements x)) env scan))
        ((and (consp x) (%token-named-p (car x) "SETF"))
         (let ((name (%function-name x scan)))
           (cond ((null name) (%star scan))
                 ((%env-fn-p env name) nil)
                 (t (%emit scan (concatenate 'string "lisp:calls " name))))))
        (t (%star scan))))

(defun %walk-function-argument (x env scan)
  "X in a function-designator position: 'sym and #'sym name the function; a
computed function is lisp:*."
  (let ((quoted (%quoted-token x scan)))
    (cond (quoted (%walk-function quoted env scan))
          ((and (consp x) (member (%head-key x scan) '("cl:function" "cl:lambda") :test #'equal))
           (%walk x env scan))
          (t (%walk x env scan) (%star scan)))))

(defun %fn-keyword-p (x)
  (and (%keyword-token-p x) (%token-named-p x "KEY" "TEST" "TEST-NOT")))

(defun %walk-args (args env scan &key fn-args mutates keys)
  "Walk ARGS of a modelled function: MUTATES (indices or :ALL-BUT-LAST) are
modified objects, FN-ARGS (indices) and, with KEYS, :key / :test values are
function designators."
  (let ((last (1- (length args))) (prev nil))
    (loop for a in args
          for i from 0
          do (cond ((or (and (listp mutates) (member i mutates))
                        (and (eq mutates :all-but-last) (< i last)))
                    (%root a env scan))
                   ((or (member i fn-args) (and keys (%fn-keyword-p prev)))
                    (%walk-function-argument a env scan))
                   (t (%walk a env scan)))
             (setf prev a))))

;;; Places and mutated objects

(defun %fresh-token-p (x env scan)
  (and (%token-p x) (eq (%env-var env (%sym x scan)) :fresh)))

(defun %constructor-call-p (x env scan)
  (let ((key (%head-key x scan)))
    (and (stringp key) (not (%env-fn-p env key)) (gethash key *scan-constructors*) t)))

(defun %root (x env scan)
  "X is an object being modified: nothing when it is fresh, lisp:mutates when
it is a free variable, lisp:* otherwise."
  (let ((*scan-depth* (1+ *scan-depth*)))
    (cond ((> *scan-depth* *scan-max-depth*) (%star scan))
          ((%token-p x)
           (if (%constant-token-p x)
               (%star scan)
               (let* ((key (%sym x scan)) (kind (%env-var env key)))
                 (cond ((eq kind :fresh) nil)
                       ((and (consp kind) (eq (car kind) :alias))
                        (%root (cadr kind) (cddr kind) scan))
                       (kind (%star scan))
                       (t (%emit scan (concatenate 'string "lisp:mutates " key)))))))
          ((consp x)
           (let* ((items (%elements x))
                  (key (%op-key (first items) scan))
                  (root (and (stringp key) (not (%env-fn-p env key)) (gethash key *scan-accessors*))))
             (cond ((%constructor-call-p x env scan) (%walk x env scan))
                   ((equal key "cl:the") (%root (third items) env scan))
                   (root (%walk-place-args (rest items) root env scan))
                   (t (%walk x env scan) (%star scan)))))
          (t (%star scan)))))

(defun %walk-place-args (args root env scan)
  "Arguments of an accessor place: the ROOT-th is the object, the rest forms."
  (loop for a in args
        for i from 0
        do (if (= i root) (%root a env scan) (%walk a env scan)))
  (when (>= root (length args)) (%star scan)))

(defun %assign (place env scan)
  "PLACE is assigned (setf, incf, push …)."
  (let ((*scan-depth* (1+ *scan-depth*)))
    (cond ((> *scan-depth* *scan-max-depth*) (%star scan))
          ((%token-p place)
           (if (%constant-token-p place)
               (%star scan)
               (let* ((key (%sym place scan)) (kind (%env-var env key)))
                 (cond ((and (consp kind) (eq (car kind) :alias))
                        (%root (cadr kind) (cddr kind) scan))
                       (kind nil)
                       (t (%emit scan (concatenate 'string "lisp:mutates " key)))))))
          ((consp place)
           (let* ((items (%elements place))
                  (args (rest items))
                  (key (%op-key (first items) scan)))
             (cond ((member key '("cl:fdefinition" "cl:symbol-function") :test #'equal)
                    (let ((name (%quoted-token (first args) scan)))
                      (if name
                          (%emit scan (concatenate 'string "lisp:defines " (%sym name scan)))
                          (progn (%walk-forms args env scan) (%star scan)))))
                   ((equal key "cl:symbol-value")
                    (let ((name (%quoted-token (first args) scan)))
                      (if name
                          (%emit scan (concatenate 'string "lisp:mutates " (%sym name scan)))
                          (progn (%walk-forms args env scan) (%star scan)))))
                   ((equal key "cl:the") (%assign (second args) env scan))
                   ((or (not (stringp key)) (%env-fn-p env key))
                    (%walk-forms args env scan) (%star scan))
                   ((gethash key *scan-accessors*)
                    (%walk-place-args args (gethash key *scan-accessors*) env scan))
                   (t (%emit scan (format nil "lisp:calls (setf ~A)" key))
                      (if args
                          (progn (%root (first args) env scan) (%walk-forms (rest args) env scan))
                          (%star scan))))))
          (t (%star scan)))))

(defparameter +scan-assignment-places+
  '(("cl:setq" . :pairs) ("cl:setf" . :pairs) ("cl:psetq" . :pairs) ("cl:psetf" . :pairs)
    ("cl:incf" 0) ("cl:decf" 0) ("cl:pop" 0) ("cl:remf" 0) ("cl:push" 1) ("cl:pushnew" 1)
    ("cl:shiftf" . :all) ("cl:rotatef" . :all) ("cl:multiple-value-setq" . :vars))
  "Assignment operators and which of their arguments are places.")

(defun %assigned-in-p (key forms scan)
  "True when a variable named KEY is assigned anywhere in FORMS (syntactic,
shadowing ignored: conservative)."
  (let ((budget 20000))
    (labels ((target-p (x) (and (%token-p x) (string= (%sym x scan) key)))
             (walk (x depth)
               (when (and (consp x) (< depth *scan-max-depth*) (plusp (decf budget)))
                 (let* ((items (%elements x))
                        (spec (cdr (assoc (%op-key (first items) scan) +scan-assignment-places+
                                          :test #'equal)))
                        (args (rest items)))
                   (when (and spec
                              (case spec
                                (:pairs (loop for (p) on args by #'cddr thereis (target-p p)))
                                (:all (some #'target-p args))
                                (:vars (some #'target-p (%elements (first args))))
                                (t (target-p (nth (first spec) args)))))
                     (return-from %assigned-in-p t))
                   (dolist (i items) (walk i (1+ depth)))))))
      (dolist (f forms nil) (walk f 0)))))

;;; Binding forms

(defun %bind-var (env token scan &optional (kind :lexical))
  (%env-bind env (%sym token scan) (if (%earmuffed-p token) :dynamic kind)))

(defun %lambda-keyword-mode (token)
  (let ((n (src:symbol-token-name token)))
    (cond ((string-equal n "&OPTIONAL") :opt)
          ((string-equal n "&KEY") :key)
          ((string-equal n "&AUX") :aux)
          ((member n '("&REST" "&BODY" "&WHOLE" "&ENVIRONMENT") :test #'string-equal) :req)
          (t :skip))))

(defun %lambda-keyword-p (x)
  (and (%token-p x)
       (let ((n (src:symbol-token-name x)))
         (and (plusp (length n)) (char= (char n 0) #\&)))))

(defun %bind-lambda-list (ll env scan &optional (kind :ordinary))
  "ENV with the variables of lambda list LL bound; init forms are walked.
KIND is :ORDINARY, :MACRO (destructuring) or :METHOD (specialized)."
  (let ((*scan-depth* (1+ *scan-depth*)))
    (when (> *scan-depth* *scan-max-depth*)
      (%star scan)
      (return-from %bind-lambda-list env))
    (multiple-value-bind (items proper tail) (%elements ll)
      (declare (ignore proper))
      (let ((mode :req))
        (dolist (x items)
          (cond ((%lambda-keyword-p x) (setf mode (%lambda-keyword-mode x)))
                ((%token-p x) (setf env (%bind-var env x scan)))
                ((not (consp x)) nil)
                ((eq mode :req)
                 (if (eq kind :method)
                     (when (%token-p (car x)) (setf env (%bind-var env (car x) scan)))
                     (setf env (%bind-lambda-list x env scan :macro))))
                ((member mode '(:opt :key :aux))
                 (let* ((parts (%elements x))
                        (var (first parts)))
                   (%walk (second parts) env scan)
                   (when (and (eq mode :key) (consp var))
                     (setf var (second (%elements var))))
                   (cond ((%token-p var) (setf env (%bind-var env var scan)))
                         ((consp var) (setf env (%bind-lambda-list var env scan :macro))))
                   (when (%token-p (third parts))
                     (setf env (%bind-var env (third parts) scan)))))))
        (when (%token-p tail) (setf env (%bind-var env tail scan)))))
    env))

(defun %walk-lambda (ll-and-body env scan &optional (kind :ordinary))
  "(lambda-list . body)."
  (let ((inner (%bind-lambda-list (first ll-and-body) env scan kind)))
    (%walk-forms (rest ll-and-body) inner scan)))

(defun %walk-let (args env scan sequential)
  (let ((inner env))
    (dolist (b (%elements (first args)))
      (multiple-value-bind (var init)
          (cond ((%token-p b) (values b nil))
                ((consp b) (let ((parts (%elements b))) (values (first parts) (second parts))))
                (t (values nil nil)))
        (%walk init (if sequential inner env) scan)
        (if (%token-p var)
            (setf inner (%bind-var inner var scan
                                   (if (and (%constructor-call-p init env scan)
                                            (not (%assigned-in-p (%sym var scan) args scan)))
                                       :fresh
                                       :lexical)))
            (%star scan))))
    (%walk-forms (rest args) inner scan)))

(defun %walk-flet (args env scan recursive)
  (let* ((defs (remove-if-not #'consp (%elements (first args))))
         (names (loop for d in defs
                      for n = (%function-name (car d) scan)
                      when n collect n))
         (inner (%env-bind-fns env names)))
    (dolist (d defs)
      (%walk-lambda (rest (%elements d)) (if recursive inner env) scan))
    (%walk-forms (rest args) inner scan)))

(defun %bind-pattern-tokens (pattern env scan)
  "ENV with every token in PATTERN bound (loop destructuring)."
  (let ((budget 1000))
    (labels ((walk (x depth)
               (cond ((%token-p x)
                      (unless (%constant-token-p x) (setf env (%bind-var env x scan))))
                     ((and (consp x) (< depth 50) (plusp (decf budget)))
                      (multiple-value-bind (items proper tail) (%elements x)
                        (declare (ignore proper))
                        (dolist (i items) (walk i (1+ depth)))
                        (when tail (walk tail (1+ depth))))))))
      (walk pattern 0))
    env))

(defun %walk-loop (args env scan)
  (let ((consumed '()) (inner env))
    ;; pass 1: binding patterns
    (loop for (x next) on args
          for i from 0
          when (and (%token-p x) (%token-named-p x "FOR" "AS" "WITH" "INTO" "NAMED" "USING" "OF-TYPE"))
            do (push (1+ i) consumed)
               (when (and (%token-named-p x "FOR" "AS" "WITH" "INTO" "USING") next)
                 (setf inner (%bind-pattern-tokens next inner scan))))
    ;; pass 2: the forms
    (loop for x in args
          for i from 0
          unless (or (member i consumed)
                     (and (%token-p x)
                          (member (src:symbol-token-name x) +scan-loop-keywords+ :test #'string-equal)))
            do (%walk x inner scan))))

;;; Format

(defun %format-control-star-p (string)
  "True when the FORMAT control STRING has a ~/ (calls a named function) or
~? (takes a control from the arguments) directive."
  (let ((i 0) (n (length string)))
    (loop while (< i n)
          do (if (char= (char string i) #\~)
                 (progn
                   (incf i)
                   (loop while (< i n)
                         do (let ((c (char string i)))
                              (cond ((char= c #\') (incf i 2))
                                    ((or (digit-char-p c) (find c ",#vV:@+-")) (incf i))
                                    (t (return)))))
                   (when (< i n)
                     (when (find (char string i) "/?") (return-from %format-control-star-p t))
                     (incf i)))
                 (incf i)))
    nil))

(defun %walk-stream-destination (dest env scan &key missing-is-io)
  "DEST is a stream argument: a fresh string stream is pure, anything else I/O."
  (cond ((and (null dest) missing-is-io) (%emit scan "lisp:io"))
        ((%fresh-token-p dest env scan) nil)
        (t (%emit scan "lisp:io") (%walk dest env scan))))

;;; Package designators

(defun %designator-package (x scan &key literal)
  "Canonical package name of the designator X, or NIL when it is computed.
LITERAL: X is not evaluated (defpackage, in-package), so any symbol names one."
  (cond ((stringp x) (%canonical-package x))
        ((and (%token-p x)
              (or literal (member (src:symbol-token-package x) '(nil :keyword))))
         (%canonical-package (src:symbol-token-name x)))
        ((%quoted-token x scan)
         (%canonical-package (src:symbol-token-name (%quoted-token x scan))))
        ((characterp x) (%canonical-package (string x)))
        (t nil)))

(defun %emit-package (designator scan &key literal missing)
  "lisp:package for DESIGNATOR (MISSING: the scan's package), lisp:* when computed."
  (if missing
      (%emit scan (concatenate 'string "lisp:package " (scan-package scan)))
      (let ((name (%designator-package designator scan :literal literal)))
        (if name
            (%emit scan (concatenate 'string "lisp:package " name))
            (%star scan)))))

;;; Definitions

(defun %emit-defines (name scan)
  (let ((key (%function-name name scan)))
    (if key
        (%emit scan (concatenate 'string "lisp:defines " key))
        (%star scan))))

(defun %walk-slot-options (slot env scan)
  "Walk the :initform of a defclass / define-condition / defstruct slot spec."
  (when (consp slot)
    (loop for (k v) on (rest (%elements slot)) by #'cddr
          when (%token-named-p k "INITFORM") do (%walk v env scan))))

;;; Operators

(define-scan-operator ("quote") (args env scan) nil)

(define-scan-operator ("function") (args env scan)
  (%walk-function (first args) env scan))

(define-scan-operator ("declare" "go") (args env scan) nil)

(define-scan-operator ("progn" "if" "when" "unless" "and" "or" "prog1" "prog2"
                       "multiple-value-prog1" "multiple-value-call" "unwind-protect" "catch"
                       "throw" "ignore-errors" "multiple-value-list" "nth-value" "return"
                       "locally" "load-time-value")
    (args env scan)
  (%walk-forms args env scan))

(define-scan-operator ("block" "return-from" "the" "eval-when") (args env scan)
  (%walk-forms (rest args) env scan))

(define-scan-operator ("tagbody") (args env scan)
  (dolist (a args) (when (consp a) (%walk a env scan))))

(define-scan-operator ("cond") (args env scan)
  (dolist (clause args)
    (if (consp clause) (%walk-forms (%elements clause) env scan) (%star scan))))

(define-scan-operator ("case" "ecase" "typecase" "etypecase") (args env scan)
  (%walk (first args) env scan)
  (dolist (clause (rest args))
    (if (consp clause) (%walk-forms (rest (%elements clause)) env scan) (%star scan))))

(define-scan-operator ("setq" "setf" "psetq" "psetf") (args env scan)
  (loop for (place value) on args by #'cddr
        do (%assign place env scan)
           (%walk value env scan))
  (when (oddp (length args)) (%star scan)))

(define-scan-operator ("incf" "decf" "pop" "remf") (args env scan)
  (if args (%assign (first args) env scan) (%star scan))
  (%walk-forms (rest args) env scan))

(define-scan-operator ("push" "pushnew") (args env scan)
  (%walk (first args) env scan)
  (if (rest args) (%assign (second args) env scan) (%star scan))
  (%walk-args (cddr args) env scan :keys t))

(define-scan-operator ("shiftf") (args env scan)
  (dolist (p (butlast args)) (%assign p env scan))
  (%walk (car (last args)) env scan))

(define-scan-operator ("rotatef") (args env scan)
  (dolist (p args) (%assign p env scan)))

(define-scan-operator ("multiple-value-setq") (args env scan)
  (dolist (v (%elements (first args))) (%assign v env scan))
  (%walk-forms (rest args) env scan))

(define-scan-operator ("let") (args env scan) (%walk-let args env scan nil))
(define-scan-operator ("let*") (args env scan) (%walk-let args env scan t))
(define-scan-operator ("flet") (args env scan) (%walk-flet args env scan nil))
(define-scan-operator ("labels") (args env scan) (%walk-flet args env scan t))
(define-scan-operator ("lambda") (args env scan) (%walk-lambda args env scan))

(define-scan-operator ("macrolet" "symbol-macrolet" "progv") (args env scan)
  ;; local macros and dynamic bindings of computed symbols: not modelled
  (%star scan))

(define-scan-operator ("dolist" "dotimes") (args env scan)
  (let* ((spec (%elements (first args)))
         (var (first spec)))
    (%walk (second spec) env scan)
    (let ((inner (if (%token-p var) (%bind-var env var scan) (progn (%star scan) env))))
      (%walk-forms (cddr spec) inner scan)
      (%walk-forms (rest args) inner scan))))

(defun %walk-do (args env scan sequential)
  (let ((specs (%elements (first args))) (inner env))
    (dolist (s specs)
      (let* ((parts (if (consp s) (%elements s) (list s)))
             (var (first parts)))
        (%walk (second parts) (if sequential inner env) scan)
        (if (%token-p var) (setf inner (%bind-var inner var scan)) (%star scan))))
    (dolist (s specs)
      (when (consp s) (%walk (third (%elements s)) inner scan)))
    (%walk-forms (%elements (second args)) inner scan)
    (%walk-forms (cddr args) inner scan)))

(define-scan-operator ("do") (args env scan) (%walk-do args env scan nil))
(define-scan-operator ("do*") (args env scan) (%walk-do args env scan t))

(define-scan-operator ("multiple-value-bind") (args env scan)
  (%walk (second args) env scan)
  (let ((inner env))
    (dolist (v (%elements (first args)))
      (when (%token-p v) (setf inner (%bind-var inner v scan))))
    (%walk-forms (cddr args) inner scan)))

(define-scan-operator ("destructuring-bind") (args env scan)
  (%walk (second args) env scan)
  (%walk-forms (cddr args) (%bind-lambda-list (first args) env scan :macro) scan))

(define-scan-operator ("handler-case") (args env scan)
  (%walk (first args) env scan)
  (dolist (clause (rest args))
    (if (consp clause)
        (let ((parts (%elements clause)))
          (%walk-lambda (rest parts) env scan))
        (%star scan))))

(define-scan-operator ("handler-bind" "restart-bind") (args env scan)
  (dolist (binding (%elements (first args)))
    (if (consp binding)
        (let ((parts (%elements binding)))
          (%walk-function-argument (second parts) env scan)
          (%walk-forms (cddr parts) env scan))
        (%star scan)))
  (%walk-forms (rest args) env scan))

(define-scan-operator ("restart-case") (args env scan)
  (%walk (first args) env scan)
  (dolist (clause (rest args))
    (if (consp clause)
        (%walk-lambda (rest (%elements clause)) env scan)
        (%star scan))))

(define-scan-operator ("with-output-to-string" "with-input-from-string") (args env scan)
  (let* ((spec (%elements (first args)))
         (var (first spec)))
    (%walk-forms (rest spec) env scan)
    (%walk-forms (rest args)
                 (if (%token-p var) (%bind-var env var scan :fresh) (progn (%star scan) env))
                 scan)))

(define-scan-operator ("with-open-file" "with-open-stream") (args env scan)
  (%emit scan "lisp:io")
  (let* ((spec (%elements (first args)))
         (var (first spec)))
    (%walk-forms (rest spec) env scan)
    (%walk-forms (rest args)
                 (if (%token-p var) (%bind-var env var scan) (progn (%star scan) env))
                 scan)))

(define-scan-operator ("with-slots" "with-accessors") (args env scan)
  (let ((instance (second args)) (inner env))
    (%walk instance env scan)
    (dolist (entry (%elements (first args)))
      (let ((var (if (consp entry) (car entry) entry)))
        (when (%token-p var)
          (setf inner (%env-bind inner (%sym var scan) (list* :alias instance env))))))
    (%walk-forms (cddr args) inner scan)))

(define-scan-operator ("loop") (args env scan) (%walk-loop args env scan))

(define-scan-operator ("in-package") (args env scan)
  (%emit scan "lisp:mutates cl:*package*"))

(define-scan-operator ("set" "makunbound") (args env scan)
  (let ((name (%quoted-token (first args) scan)))
    (if name
        (%emit scan (concatenate 'string "lisp:mutates " (%sym name scan)))
        (progn (%walk (first args) env scan) (%star scan)))
    (%walk-forms (rest args) env scan)))

(define-scan-operator ("symbol-value") (args env scan)
  (let ((name (%quoted-token (first args) scan)))
    (if name
        (%emit scan (concatenate 'string "lisp:reads " (%sym name scan)))
        (progn (%walk (first args) env scan) (%star scan)))))

(define-scan-operator ("fmakunbound") (args env scan)
  (let ((x (first args)))
    (if (and (consp x) (equal (%head-key x scan) "cl:quote"))
        (%emit-defines (second (%elements x)) scan)
        (progn (%walk x env scan) (%star scan)))))

(define-scan-operator ("eval" "compile") (args env scan)
  (%walk-forms args env scan)
  (%star scan))

(define-scan-operator ("coerce") (args env scan)
  (%walk-forms args env scan)
  (let ((type (%quoted-token (second args) scan)))
    (when (or (null type) (%token-named-p type "FUNCTION" "COMPILED-FUNCTION"))
      (%star scan))))

(define-scan-operator ("format") (args env scan)
  ;; destination NIL: a string, no I/O
  (unless (and args (%nil-p (first args)))
    (%walk-stream-destination (first args) env scan :missing-is-io t))
  (let ((control (second args)))
    (cond ((stringp control) (when (%format-control-star-p control) (%star scan)))
          (t (%walk control env scan) (%star scan))))
  (%walk-forms (cddr args) env scan))

(define-scan-operator ("write") (args env scan)
  (let ((stream (loop for (k v) on (rest args) by #'cddr
                      when (%token-named-p k "STREAM") return (list v))))
    (if stream
        (%walk-stream-destination (first stream) env scan)
        (%emit scan "lisp:io"))
    (%walk-forms args env scan)))

;;; Definitions

(define-scan-operator ("defun" "defmacro") (args env scan)
  (%emit-defines (first args) scan)
  (%walk-lambda (rest args) env scan))

(define-scan-operator ("defmethod") (args env scan)
  (%emit-defines (first args) scan)
  (let ((rest (rest args)))
    (loop while (and rest (not (listp (first rest)))) do (pop rest))
    (%walk-lambda rest env scan :method)))

(define-scan-operator ("defgeneric") (args env scan)
  (%emit-defines (first args) scan)
  (dolist (option (cddr args))
    (when (and (consp option) (%token-named-p (car option) "METHOD"))
      (let ((rest (rest (%elements option))))
        (loop while (and rest (not (listp (first rest)))) do (pop rest))
        (%walk-lambda rest env scan :method)))))

(define-scan-operator ("defvar" "defparameter" "defconstant") (args env scan)
  (%emit-defines (first args) scan)
  (%walk (second args) env scan))

(define-scan-operator ("defclass" "define-condition") (args env scan)
  (%emit-defines (first args) scan)
  (dolist (slot (%elements (third args))) (%walk-slot-options slot env scan))
  (dolist (option (nthcdr 3 args))
    (when (consp option)
      (let ((parts (%elements option)))
        (cond ((%token-named-p (first parts) "DEFAULT-INITARGS")
               (loop for (nil v) on (rest parts) by #'cddr do (%walk v env scan)))
              ((and (%token-named-p (first parts) "REPORT") (consp (second parts)))
               (%walk-function-argument (second parts) env scan)))))))

(define-scan-operator ("defstruct") (args env scan)
  (let ((name (first args)))
    (%emit-defines (if (consp name) (car name) name) scan))
  (dolist (slot (rest args))
    (when (consp slot) (%walk (second (%elements slot)) env scan))))

(define-scan-operator ("deftype") (args env scan)
  (%emit-defines (first args) scan)
  (%walk-lambda (rest args) env scan :macro))

(define-scan-operator ("defpackage" "uiop:define-package") (args env scan)
  (%emit-package (first args) scan :literal t))

;;; Table-driven groups

(defun %install-scan-tables ()
  (flet ((op (name fn) (setf (gethash (%op-name name) *scan-operators*) fn))
         (kind (name k) (setf (gethash (%op-name name) *scan-fn-kinds*) k)))
    (dolist (n +scan-constructor-functions+)
      (setf (gethash (%op-name n) *scan-constructors*) t))
    (loop for (n . i) in +scan-accessor-roots+
          do (setf (gethash (%op-name n) *scan-accessors*) i))
    (dolist (n +scan-pure-functions+)
      (let ((fn-args (rest (assoc n +scan-function-arguments+ :test #'string=)))
            (keys (and (member n +scan-key-functions+ :test #'string=) t)))
        (unless (or fn-args keys) (kind n :pure))
        (unless (gethash (%op-name n) *scan-operators*)
          (op n (lambda (args env scan) (%walk-args args env scan :fn-args fn-args :keys keys))))))
    ;; funcall / apply: the function argument decides
    (dolist (n '("funcall" "apply"))
      (op n (lambda (args env scan) (%walk-args args env scan :fn-args '(0)))))
    (loop for (n . mutates) in +scan-destructive-functions+
          do (let ((fn-args (rest (assoc n +scan-function-arguments+ :test #'string=)))
                   (keys (and (member n +scan-key-functions+ :test #'string=) t))
                   (mutates (if (eq (first mutates) :all-but-last) :all-but-last mutates)))
               (op n (lambda (args env scan)
                       (%walk-args args env scan :fn-args fn-args :mutates mutates :keys keys)))))
    (loop for (n . index) in +scan-stream-functions+
          do (let ((index index))
               (kind n :io)
               (op n (lambda (args env scan)
                       (%walk-stream-destination (nth index args) env scan :missing-is-io t)
                       (loop for a in args for i from 0
                             unless (= i index) do (%walk a env scan))))))
    (kind "write" :io)
    (dolist (n +scan-io-functions+)
      (kind n :io)
      (op n (lambda (args env scan) (%emit scan "lisp:io") (%walk-forms args env scan))))
    (dolist (n +scan-read-functions+)
      (kind n :read-eval)
      (op n (lambda (args env scan) (%emit scan "lisp:read-eval") (%walk-forms args env scan))))
    (loop for (n . index) in +scan-package-functions+
          do (let ((index index))
               (op n (lambda (args env scan)
                       (if (< index (length args))
                           (let ((d (nth index args)))
                             (%emit-package d scan)
                             (unless (%designator-package d scan) (%walk d env scan)))
                           (%emit-package nil scan :missing t))
                       (loop for a in args for i from 0
                             unless (= i index) do (%walk a env scan))))))))

(%install-scan-tables)

;;; Entry point

(defun %cst-markers (forms scan)
  "Pre-pass over the CST: emit lisp:read-eval for #. and #S. → T when a #n= /
#n# label is present (its value may be circular)."
  (src:walk-nodes
   (lambda (node)
     (case (src:node-kind node)
       ((:label-definition :label-reference) (return-from %cst-markers t))
       (:dispatch
        (let ((text (src:node-text node)))
          (when (or (src:read-eval-placeholder-p (src:node-value node))
                    (src:structure-placeholder-p (src:node-value node))
                    (and (>= (length text) 2) (char= (char text 0) #\#)
                         (char-equal (char text 1) #\S)))
            (%emit scan "lisp:read-eval"))))
       (t (when (or (src:read-eval-placeholder-p (src:node-value node))
                    (src:structure-placeholder-p (src:node-value node)))
            (%emit scan "lisp:read-eval")))))
   forms)
  nil)

(defun %scan-toplevel (form env scan)
  (%walk form env scan)
  ;; a top-level IN-PACKAGE switches the package of the forms after it
  (when (equal (%head-key form scan) "cl:in-package")
    (let ((name (%designator-package (second (%elements form)) scan :literal t)))
      (when name (setf (scan-package scan) name)))))

(defun %form-resources (code package features)
  (let ((parsed (if features
                    (src:read-source code :features features)
                    (src:read-source code))))
    (if (not (src:parsed-source-clean-p parsed))
        (list "lisp:*")
        (let ((forms (src:parsed-source-forms parsed))
              (scan (%make-scan (%canonical-package package)))
              (env (%make-env))
              (*scan-depth* 0))
          (if (%cst-markers forms scan)
              (list "lisp:*")
              (progn
                (dolist (f forms) (%scan-toplevel (src:node-value f) env scan))
                (%finish scan (length forms))))))))

(defun form-resources (code &key (package "cl-user") features)
  "→ permission resource strings for evaluating CODE (a string of zero or
more forms) in PACKAGE: \"lisp:pure\" alone, or \"lisp:reads <sym>\",
\"lisp:defines <sym>\", \"lisp:mutates <sym>\", \"lisp:package <name>\",
\"lisp:io\", \"lisp:read-eval\", \"lisp:calls <sym>\" in order of appearance
without duplicates, then \"lisp:*\" when anything was not modelled. NIL for
blank or comment-only CODE. FEATURES (keywords) decide #+/#-; NIL means the
host *FEATURES*. Never signals."
  ;; The walk is total by construction; this is the backstop for a reader
  ;; or host failure (a SERIOUS-CONDITION includes stack exhaustion).
  (handler-case (%form-resources code package features)
    (serious-condition () (list "lisp:*"))))
