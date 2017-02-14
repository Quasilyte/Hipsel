(require 'dash)

;; {{ BUGS }}

;; - Need to check if package is opened in every expr

;; {{ NOTES }}

;; Packages are currently `closed'. User can not extend them.
;; It is also kinda tricky to write multi-file packages.

;; Markers used in this project:
;;   #PERFOMANCE marks code that may be a subject to optimizations
;;   #FIXME marks something that should be fixed (bad code)

;; {{ UTILS }}

(defalias 'set! 'setq)

(defmacro error-unless (cond-expr fmt-str &rest fmt-args)
  (declare (indent defun))
  `(unless ,cond-expr
     (error ,fmt-str ,@fmt-args)))

(defmacro error-when (cond-expr fmt-str &rest fmt-args)
  (declare (indent defun))
  `(when ,cond-expr
     (error ,fmt-str ,@fmt-args)))

;; {{ GLOBAL STATE }}

;; All varibales below should be buffer-local
(defvar-local hel-OPENED-PKG nil
  "Current package symbol")
(defvar-local hel-OPENED-SYMBOLS (make-hash-table :test #'eq)
  "Symbols visible to current package. Maps local symbols to private symbols")
(defvar-local hel-PKG-MAP (make-hash-table :test #'eq)
  "Global package index. Maps package name to exported private symbols vector")
(defvar-local hel-ALIASES
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
        (sym-pairs '((set! setq)
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
                     (throw throw))
    (dolist (sym-pair sym-pairs)
      (setq hel-sym (nth 0 sym-pair))
      (setq elisp-sym (nth 1 sym-pair))
      (puthash hel-sym elisp-sym alias-table))
    alias-table)
  "Symbols that have 1-1 mapping with Emacs Lisp.
We need those mostly due perfomance reasons: `defalias'
causes slowdown even in byte-compiled code.
If we use `disassemble', `car' compiles to `car',
but alias is looked up dynamically.")

;; {{ CORE DEFS }}

(defun hel--intern (pkg sym)
  (intern (format "_hel-%s:%s" pkg sym)))

(defun hel--priv-sym-pkg (priv-sym)
  ;; #PERFOMANCE: this function can be optimized
  (let* ((sym-name (symbol-name priv-sym))
         (pkg-name (substring sym-name
                              (length "_hel-")
                              (string-match ":" sym-name))))
    (intern pkg-name)))

(defun hel--priv-sym-pub-name (priv-sym)
  ;; #PERFOMANCE: this function can be optimized
  (let ((sym-name (symbol-name priv-sym)))
    (substring sym-name (1+ (string-match ":" sym-name)))))

;; {{ PACKAGE-RELATED }}

(defmacro hel-pkg-open! (pkg)
  (error-when (eq hel-OPENED-PKG pkg)
    "Package `%s' is already opened" pkg)
  (set! hel-OPENED-PKG pkg
        hel-OPENED-SYMBOLS (make-hash-table :test #'eq))
  nil)

(defmacro hel-pkg-export! (&rest symbols)
  (error-unless symbols
    "Can not export a package without any symbols")
  (let ((pkg-symbols (make-vector (length symbols) nil))
        (priv-sym nil)
        (sym-pkg "")
        (i 0))
    (dolist (sym symbols)
      (setq priv-sym (gethash sym hel-OPENED-SYMBOLS))
      (error-unless priv-sym
        "Symbol `%s' not found in opened package" sym)
      (setq sym-pkg (hel--priv-sym-pkg priv-sym))
      (error-unless (eq hel-OPENED-PKG sym-pkg)
        "Symbol `%s' belongs to package `%s' and can not be re-exported"
        sym sym-pkg)
      (aset pkg-symbols i (hel--intern hel-OPENED-PKG sym))
      (setq i (1+ i))) 
    (puthash hel-OPENED-PKG pkg-symbols hel-PKG-MAP))
  nil)

(defun hel--import-sym! (sym priv-sym)
  (puthash sym priv-sym hel-OPENED-SYMBOLS))

(defun hel--import-priv-sym! (prefix priv-sym)
  (hel--import-sym! (intern (concat prefix (hel--priv-sym-pub-name priv-sym)))
                    priv-sym))

(defun hel--import-pub-sym! (prefix sym priv-sym)
  (hel--import-sym! (intern (concat prefix (symbol-name sym)))
                    priv-sym))

(defun hel--pkg-import-all! (src-pkg prefix src-symbols)
  (dovector (priv-sym src-symbols)
    (hel--import-priv-sym! prefix priv-sym)))

(defun hel--pkg-import-list! (src-pkg prefix src-symbols import-symbols)
  (dolist (sym import-symbols)
    (let ((priv-sym (hel--intern src-pkg sym)))
      (error-unless (vec:contains? src-symbols priv-sym)
        "Symbol `%s' is not exported by package `%s'"
        sym src-pkg)
      (hel--import-pub-sym! prefix sym priv-sym))))

(defmacro hel-pkg-import! (src-pkg prefix &rest import-symbols)
  (let ((src-symbols (gethash src-pkg hel-PKG-MAP)))
    (error-unless src-symbols
      "Package `%s' not found" src-pkg)
    (if import-symbols
        (hel--pkg-import-list! src-pkg prefix src-symbols import-symbols)
      (hel--pkg-import-all! src-pkg prefix src-symbols))))
    
(defun hel--pkg-define! (def sym params forms)
  (error-unless hel-OPENED-PKG
    "No package is opened, can not define symbol `%s'" sym)
  (let ((priv-sym (hel--intern hel-OPENED-PKG sym)))
    (puthash sym priv-sym hel-OPENED-SYMBOLS)
    `(,def ,priv-sym ,params ,@(-map #'hel-form forms))))

(defmacro hel-pkg-defun! (sym params &rest forms)
  (declare (indent defun))
  (hel--pkg-define! 'defun sym params forms))

(defmacro hel-pkg-defmacro! (sym params &rest forms)
  (declare (indent defun))
  (hel--pkg-define! 'defmacro sym params forms))

;; {{ EVAL-RELATED }}

;; - eval interactive like `eval-last-sexp', `eval-region', etc
;; - byte compile file for efficient importing as library
;; - dynamic `load'
;; - macro expansions

(defun hel-form (form)
  (cond
   ((listp form) (hel-form:list form))
   (t form)))

(defun hel-form:list (list)
  (let* ((head (car list))
         (tail (cdr list))
         (alias (gethash head hel-ALIASES)))
    (if alias
        (cons alias (-map #'hel-form tail))
      (cond ((eq 'elisp head) (hel-form:elisp tail))
            ((eq 'quote head) (hel-form:quoted (car tail)))
            ((eq 'package head) (cons 'hel-pkg-open! tail))
            ((eq 'export head) (cons 'hel-pkg-export! tail))
            ((eq 'import head) (cons 'hel-pkg-import! tail))
            ((eq 'def/fn head) (hel-form:def/callable 'hel-pkg-defun! tail))
            ((eq 'def/macro head) (hel-form:def/callable 'hel-pkg-defmacro! tail))
            ((eq 'def/var head) (hel-form:def/val 'defvar tail))
            ((eq 'def-local/var head) (hel-form:def/val 'defvar-local tail))
            ((symbolp head) (hel-form:call head tail))
            (t (error "Unexpected head of unquoted list"))))))

(defun hel-form:elisp (forms)
  `(progn ,@forms))

(defun hel-form:def/callable (def forms)
  (let* ((signature (car forms))
         (sym (car signature))
         (params (cdr signature))
         (forms (cdr forms)))
    `(,def ,sym ,params ,@forms)))

(defun hel-form:def/val (def forms)
  (let ((sym (hel--intern hel-OPENED-PKG (nth 0 forms)))
        (val (nth 1 forms)))
    `(,def ,sym ,(hel-form val) nil)))

(defun hel-form:call (fn args)
  (let ((sym (gethash fn hel-OPENED-SYMBOLS)))
    (error-unless sym
      "`%s' is undefined and can not be called" fn)
    (if (macrop sym)
        (hel-form (macroexpand (cons sym (-map #'hel-form:quoted args))))
      (cons sym (-map #'hel-form args)))))

(defun hel-form:quoted (form)
  (if (vectorp form)
      (error "no infix forms yet")
    form))

;; {{ SANDBOX }}

(defmacro EVAL (form)
  (hel-form form))

(defmacro call (f &rest args)
  (cons (gethash f hel-OPENED-SYMBOLS f) args))

(defmacro quote-all (&rest forms) (declare (indent defun)))

;; (EVAL (def/var x (add1 1)))

(quote-all
  (hel-pkg-open! mod-a)
  (hel-pkg-defun! add1 (x) (+ 1 x))
  (hel-pkg-defun! add2 (x) (call add1 (call add1 x)))
  (hel-pkg-defmacro! macros (x) (list 'quote (cons (cdr x) (car x))))
  
  (hel-pkg-defmacro! fncallx (f suffix &rest args)
    (let ((name (intern (format "%s%s" f suffix))))
      `(,name ,@args)))
  (hel-pkg-defmacro! macro-add1 (x)
    `(add1 ,x))
  
  (hel-pkg-export! add1 add2)
  (hel-pkg-close!)

  (call add1 1)
  
  ;; defined `mod-a.add1'

  (hel-pkg-open! mod-b)
  (hel-pkg-import! mod-a "" add1)
  
  (message "%s" (add1 1))
  (hel-pkg-export! add1)
  (hel-pkg-close!))

