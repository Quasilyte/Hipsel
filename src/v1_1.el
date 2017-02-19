;;; -*- lexical-binding: t -*-

(require 'dash) 

(defalias 'set! 'setq)

;; {{ BUGS }}

;; - Need to check if package is opened in every expr
;; - Def/* forms should not return mangled symbol
;; - `set!' and some other forms must be forbidden at top level

;; {{ GLOBAL STATE }}

(defvar-local hel-OPENED-PKG nil
  "Current package symbol")
(defvar-local hel-OPENED-SYMBOLS (make-hash-table :test #'eq)
  "Symbols visible to current package. Maps local symbols to private symbols")
(defvar-local hel-PKG-MAP (make-hash-table :test #'eq)
  "Global package index. Maps package name to exported private symbols vector")
(defconst hel-ALIASES
  ;; #PERFOMANCE: there are more primitive functions to be added in this list
  ;; Missing: (`memq' `symbol-value' `symbol-function'
  ;;           `set' `setf' `get' point goto-char insert
  ;;           point-max point-min
  ;;           char-after following-char preceding-char
  ;;           current-column indent-to
  ;;           eolp eobp bolp bobp
  ;;           current-buffer set-buffer save-current-buffer
  ;;           interactive-p
  ;;           forward-char forward-word
  ;;           skip-chars-forward skip-chars-backward
  ;;           forward-line char-syntax
  ;;           buffer-substring delete-region
  ;;           narrow-to-region widen
  ;;           end-of-line save-execursion
  ;;           save-window-excursion
  ;;           save-restriction
  ;;           `unwind-protect' `condition-case'
  ;;           temp-output-buffer-setup temp-output-buffer-show
  ;;           `string-equal' `member' `assq'
  ;;           `nreverse' `setcar' `setcdr'
  ;;           `car-safe' `cdr-safe' `nconc')
  (let (hel-sym   
        elisp-sym
        (alias-table (make-hash-table :test #'eq))
        (sym-pairs '((nil? null)
                     (array.set! aset)
                     (array.nth aref)
                     (array.slice substring)
                     (seq.len length)
                     (seq.nth elt) 
                     (seq.concat concat)
                     (list.nth nth)
                     (list.head car)
                     (list.tail cdr)
                     (list.drop nthcdr)
                     (list list)
                     (pair cons)
                     (sym? symbolp)
                     (pair? consp)
                     (str? stringp)
                     (list? listp)
                     (num? numberp)
                     (int? integerp)
                     (eq? eq)
                     (equal? equal)
                     (not not)
                     (inc 1+)
                     (dec 1-)
                     (- -)
                     (+ +)
                     (* *)
                     (/ /)
                     (% %)
                     (> >)
                     (< <)
                     (>= >=)
                     (<= <=)
                     (= =)
                     (max-of max) 
                     (min-of min)
                     (catch catch)
                     (throw throw))))
    (dolist (sym-pair sym-pairs)
      (set! hel-sym (nth 0 sym-pair))
      (set! elisp-sym (nth 1 sym-pair))
      (puthash hel-sym elisp-sym alias-table))
    alias-table)
  "Symbols that have 1-1 mapping with Emacs Lisp.
We need those mostly due perfomance reasons: `defalias'
causes slowdown even in byte-compiled code.
If we use `disassemble', `car' compiles to `car',
but alias is looked up dynamically.")

;; {{ UTILS }}

(defmacro let/list (vars list-expr &rest forms)
  (declare (indent 2))
  (let ((lst (make-symbol "lst"))
        (bindings nil)
        (n 0))
    (dolist (var vars)
      (setq bindings (cons `(,var (nth ,n ,lst))
                           bindings)
            n (1+ n)))
    `(let* ((,lst ,list-expr)
            ,@bindings)
       ,@forms)))

(defun hel-def/fn (forms sym params locals)
  (if locals  
      `(defun ,sym ,params
         (let* ,(-partition 2 locals)
           ,@forms))
    `(defun ,sym ,params ,@forms)))

(defun hel-def/macro (forms sym params locals)
  (if locals
      `(defmacro ,sym ,params
         (let* ,(-partition 2 locals)
           ,@forms))
    `(defmacro ,sym ,params ,@forms)))

(defun hel-parse-signature (signature)
  (let (locals)
    (when (listp (car signature))
      (set! locals (cdr signature)
            signature (car signature)))
    (list (car signature)
          (cdr signature)
          locals)))

(defmacro def/fn (signature &rest forms)
  (declare (indent defun))
  (apply #'hel-def/fn
         (cons forms (hel-parse-signature signature))))

(defmacro def/macro (signature &rest forms)
  (declare (indent defun))
  (apply #'hel-def/macro
         (cons forms (hel-parse-signature signature))))

(def/macro (set-indent sym indent)
  `(put ',sym 'lisp-indent-function ',indent))

(def/macro ((if-let var-val then-form &rest else-forms)
            var (nth 0 var-val)
            val (nth 1 var-val))
  `(let ((,var ,val))
     (if ,var
         ,then-form
       ,@else-forms)))
(set-indent if-let 2)

;; {{ VECTOR OPS }}

(def/fn ((vector.find vec pred &optional begin end)
         pos (or begin 0)
         count (or end (length vec))
         result nil)
  (while (< pos count)
    (if (funcall pred (aref vec pos))
        (progn
          (set! result (aref vec pos))
          (set! pos count))
      (set! pos (1+ pos))))
  result)

(def/fn ((vector.contains? vec x &optional begin end)
         pos (or begin 0)
         count (or end (length vec))
         result nil)
  (while (< pos count)
    (if (equal x (aref vec pos))
        (progn
          (set! result t)
          (set! pos count))
        (set! pos (1+ pos))))
  result)

(def/macro ((dovector var-vec &rest forms)
            var (nth 0 var-vec)
            vec (nth 1 var-vec))
  `(let (,var
         (pos 0)
         (count (length ,vec)))
     (while (< pos count)
       (set! ,var (aref ,vec pos))
       ,@forms
       (set! pos (1+ pos)))))
(set-indent dovector defun)

;; {{ CORE DEFS }}

(def/fn (hel--intern pkg sym)
  (intern (format "_hel-%s:%s" pkg sym)))

(def/fn ((hel--priv-sym-pkg priv-sym)
         sym-name (symbol-name priv-sym)
         pkg-name (substring sym-name
                             (length "_hel-")
                             (string-match ":" sym-name)))
  ;; #PERFOMANCE: this function can be optimized
  (intern pkg-name))

(def/fn ((hel--priv-sym-pub-name priv-sym)
         sym-name (symbol-name priv-sym))
  (substring sym-name (1+ (string-match ":" sym-name))))

(def/fn (hel--sym-has-sigil? sigil sym)
  (= sigil (aref (symbol-name sym) 0)))

(def/fn (hel--unary-prefix? sym)
  (and (symbolp sym)
       (hel--sym-has-sigil? ?& sym)))

(def/fn (hel--unary-prefix sym)
  (and (symbolp sym)
       (not (eq '& sym))
       (let ((name (symbol-name sym)))
         (if (= ?& (aref name 0))
             (intern (substring name 1))
           nil))))

;; {{ ERROR-RELATED }}

(def/macro (error-unless cond-expr fmt-str &rest fmt-args)
  `(unless ,cond-expr
     (error ,fmt-str ,@fmt-args)))
(set-indent error-unless defun)

(def/macro (error-when cond-expr fmt-str &rest fmt-args)
  `(when ,cond-expr
     (error ,fmt-str ,@fmt-args)))
(set-indent error-when defun)

(def/fn (check-unary-prefix-arg callable arg)
  (error-unless arg
    "UPC `&%s' is missing an argument" callable)
  (error-when (hel--unary-prefix? arg)
    "UPCs are not composable: can not call `&%s' with `%s'"
    callable arg))

(def/fn (check-sym sym)
  (error-when (hel--sym-has-sigil? ?& sym)
    "Bad symbol `%s': invalid `&' prefix" sym))

(def/fn (check-pkg-openable pkg)
  (error-when (eq hel-OPENED-PKG pkg)
    "Package `%s' is already opened" pkg))

(def/fn (check-export-list export-list)
  (error-unless export-list
    "Can not export a package without any symbols"))

(def/fn (check-exportable sym priv-sym)
  (error-unless priv-sym
    "Symbol `%s' not found in opened package" sym)
  (let ((sym-pkg (hel--priv-sym-pkg priv-sym)))
    (error-unless (eq hel-OPENED-PKG sym-pkg)
      "Symbol `%s' belongs to package `%s' and can not be re-exported"
      sym sym-pkg)))

(def/fn (check-can-import-all pkg symbols)
  (error-when (> (length symbols) 100)
    "Package `%s' is too big to be imported entirely"
    pkg))

(def/fn (check-pkg-exports pkg pkg-symbols sym priv-sym)
  (error-unless (vector.contains? pkg-symbols priv-sym)
    "Symbol `%s' is not exported by package `%s'"
    sym src-pkg))

(def/fn (check-can-import-elisp-all import-symbols)
  ;; Maybe global import of Emacs Lisp symbols
  ;; will be permitted in future
  (error-unless import-symbols
    "Package `elisp' is too big to be imported entirely"))

(def/fn (check-elisp-sym-bound sym)
  (error-unless (or (fboundp sym)
                    (boundp sym))
    "Symbol `%s' is not exported by package `elisp'"
    sym))

(def/fn (check-pkg-symbols pkg symbols)
  (error-unless symbols
    "Package `%s' not found" pkg))

(def/fn (check-can-call fn sym)
  (error-unless sym
    "`%s' is undefined and can not be called" fn))

(def/fn (check-local-pair var expr locals)
  (error-unless (symbolp var)
    "Invalid local binding: `%s'" var)
  (error-when (and (not expr)
                   (= 1 (length locals)))
    "Local binding `%s' misses initialier" var))

(def/fn (blame-invalid-list-head)
  (error "Invalid unquoted list head"))

(def/fn (blame-keyword-param)
  (error "Keyword params are not supported yet"))

(def/fn (blame-invalid-param param)
  (error "Invalid formal param `%s'" param))

;; {{ TRANSFORM-RELATED }}

(def/fn (hel-transform form)  
  (cond ((null form) nil)
        ((listp form) (hel-transform/list form))
        ;; Will we use `[]' for infix notation?
        (t form)))

(def/fn ((hel-transform/list form)
         head nil
         result nil)
  (while form
    (set! head (pop form))
    (if-let (unary-prefix (hel--unary-prefix head))
        (let ((next-form (pop form)))
          (check-unary-prefix-arg unary-prefix next-form)
          (push (list unary-prefix next-form)
                result))
      (push (hel-transform head) result)))
  (nreverse result))

;; {{ EVAL-RELATED }}

(def/fn (hel-form form)
  (cond ((null form) nil)
        ((listp form) (hel-form/list form))
        (t form)))

(def/fn (hel-form/list/call-alias alias args)
  (cons alias (-map #'hel-form args)))

(def/fn ((hel-form/list/call fn args)
         sym (gethash fn hel-OPENED-SYMBOLS))
  (check-can-call fn sym)
  ;; Do we need eager macro expansion here?
  (cons sym (hel-form args)))

(def/fn ((hel-form/list form)
         head (car form)
         tail (cdr form)
         alias (gethash head hel-ALIASES))
  (cond
   (alias
    (cons alias (-map #'hel-form tail)))
   ((symbolp head)
    (pcase head
      ;; Special forms
      (`package `(hel-pkg-open! ',(car tail)))
      (`export `(hel-pkg-export! ',tail))
      (`elisp `(progn ,@tail))
      (`def/fn (hel-pkg-def/fn tail))
      ;; Normal function/macro invocation
      (_ (hel-form/list/call head tail))))
   ;; #NOTE: if we want lisp1, expressions
   ;; must be allowed in head possition
   (t (blame-invalid-list-head))))

;; {{ PACKAGE-RELATED }}

(def/fn ((hel-exportable-priv-sym sym)
         priv (gethash sym hel-OPENED-SYMBOLS))
  (check-exportable sym priv-sym)
  priv-sym)

(def/fn ((hel-pkg-symbols pkg)
         symbols (gethash pkg hel-PKG-MAP))
  (check-pkg-symbols pkg symbols)
  symbols)

(def/fn (hel-pkg-open! pkg)
  (check-pkg-openable pkg)
  (set! hel-OPENED-PKG pkg
        hel-OPENED-SYMBOLS (make-hash-table :test #'eq))
  nil)

(def/fn ((hel-pkg-export! export-symbols)
         pkg-symbols (make-vector (length export-symbols) nil)
         i 0)
  (check-export-list export-symbols)
  (dolist (export-sym export-symbols)
    (aset pkg-symbols i (hel-exportable-priv-sym export-sym))
    (set! i (1+ i)))
  (puthash hel-OPENED-PKG pkg-symbols hel-PKG-MAP)
  nil)

(def/fn (hel--import-sym! sym priv-sym)
  (puthash sym priv-sym hel-OPENED-PKG))

(def/fn (hel--import-priv-sym! prefix priv-sym)
  (hel--import-sym! (intern (concat prefix (hel--priv-sym-pub-name priv-sym)))
                    priv-sym))

(def/fn (hel--import-pub-sym! prefix sym priv-sym)
  (hel--import-sym! (intern (concat prefix (symbol-name sym)))
                    priv-sym))

(def/fn (hel--pkg-import-all! src-pkg prefix src-symbols)
  (check-can-import-all src-pkg src-symbols)
  (dovector (priv-sym src-symbols)
    (hel--import-priv-sym! prefix priv-sym)))

(def/fn (hel--pkg-import-list! src-pkg prefix src-symbols import-symbols)
  (dolist (sym import-symbols)
    (let ((priv-sym (hel--intern src-pkg sym)))
      (check-pkg-exports src-pkg src-symbols sym priv-sym)
      (hel--import-pub-sym! prefix sym priv-sym))))

(def/fn (hel--pkg-import-elisp! prefix import-symbols)
  (check-can-import-elisp-all import-symbols)
  (dolist (sym import-symbols)
    (check-elisp-sym-bound sym)
    (hel--import-pub-sym! prefix sym sym)))

(def/macro (hel-pkg-import! src-pkg prefix &rest import-symbols)
  (if (eq src-pkg 'emacs-lisp)
      (hel--pkg-import-elisp! prefix import-symbols)
    (let ((src-symbols (hel-pkg-symbols src-pkg)))
      (if import-symbols
          (hel--pkg-import-list! src-pkg prefix src-symbols import-symbols)
        (hel--pkg-import-all! src-pkg prefix src-symbols)))))

;; {{ DEF-RELATED }}

(def/fn (hel-parse-params params)
  (if params
      (let ((head (car params)))
        (cons (cond ((eq '& head) '&rest) ;; Or maybe `...' instead of `&'
                    ((keywordp head) (blame-keyword-param))
                    ((symbolp head) head)
                    (t (blame-invalid-param head)))
              (hel-parse-params (cdr params))))
    nil))
    
(def/fn (hel-parse-locals locals)
  (if locals
      (let/list (var expr) locals
        (check-local-pair var expr locals)
        (cons var
              (cons (hel-form expr)
                    (hel-parse-locals (-drop 2 locals)))))
    nil))

(def/fn ((hel-pkg-def/fn params)
         signature (car params)
         forms (cdr params))
  (let/list (sym params locals)
      (hel-parse-signature signature)
    (hel-def/fn (-map #'hel-form forms)
                (hel--intern hel-OPENED-PKG sym)
                (hel-parse-params params)
                (hel-parse-locals locals))))

