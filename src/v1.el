(require 'dash)

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

;; {{ GLOBAL STATE }}

(defconst hel-OPENED-PKG nil
  "Current package symbol")
(defconst hel-OPENED-SYMBOLS (make-hash-table :test #'eq)
  "Symbols visible to current package. Maps local symbols to private symbols")
(defconst hel-PKG-MAP (make-hash-table :test #'eq)
  "Global package index. Maps package name to exported private symbols vector")

;; {{ CORE DEFS }}

(defun hel--intern (pkg sym)
  (intern (format "__%s:%s" pkg sym)))

(defun hel--sym-pkg (sym)
  ;; #PERFOMANCE: this function can be optimized
  (let* ((sym-name (symbol-name sym))
         (pkg-name (substring sym-name 2 (string-match ":" sym-name))))
    (intern pkg-name)))

;; {{ PACKAGE-RELATED }}

(defmacro hel-pkg-open! (pkg)
  (when hel-OPENED-PKG
    (error "Already has opened package `%s'. Close it" hel-OPENED-PKG))
  (set! hel-OPENED-PKG pkg)
  nil)

(defun hel-pkg-close! ()
  (unless hel-OPENED-PKG
    (error "No package is opened to be closed"))
  (set! hel-OPENED-PKG nil)
  (clrhash hel-OPENED-SYMBOLS))

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
      (setq sym-pkg (hel--sym-pkg priv-sym))
      (error-unless (eq hel-OPENED-PKG sym-pkg)
        "Symbol `%s' belongs to package `%s' and can not be re-exported"
        sym sym-pkg)
      (aset pkg-symbols i (hel--intern hel-OPENED-PKG sym))
      (setq i (1+ i))) 
    (puthash hel-OPENED-PKG pkg-symbols hel-PKG-MAP))
  nil)

(defun hel--import-sym! (pkg prefix sym)
  (message "import `%s'.`%s'" pkg sym)
  (puthash (intern (concat prefix (symbol-name sym)))
           (hel--intern pkg sym)
           hel-OPENED-SYMBOLS))

(defun hel--pkg-import-all! (src-pkg prefix src-symbols)
  (dovector (sym src-symbols)
    (hel--import-sym! src-pkg prefix sym)))

(defun hel--pkg-import-list! (src-pkg prefix src-symbols import-symbols)
  (dolist (sym import-symbols)
    (error-unless (vec:contains? src-symbols (hel--intern src-pkg sym))
      "Symbol `%s' is not exported by package `%s'"
      sym src-pkg)
    (hel--import-sym! src-pkg prefix sym)))

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
    `(,def ,priv-sym ,params ,@forms)))

(defmacro hel-pkg-defun! (sym params &rest forms)
  (hel--pkg-define! 'defun sym params forms))

(defmacro hel-pkg-defmacro! (sym params &rest forms)
  (hel--pkg-define! 'defmacro sym params forms))

;; {{ SANDBOX }}

(defmacro call (f &rest args)
  (cons (gethash f hel-OPENED-SYMBOLS f) args))

(defmacro quote-all (&rest forms) (declare (indent defun)))

(quote-all
  (hel-pkg-open! mod-a)
  (hel-pkg-defun! add1 (x) (+ 1 x))
  (hel-pkg-defun! add2 (x) (call add1 (call add1 x)))
  (hel-pkg-defmacro! macros (x) (list 'quote (cons (cdr x) (car x))))
  (hel-pkg-export! add2)
  (hel-pkg-close!)

  (call add2 1)
  
  ;; defined `mod-a.add1'

  (hel-pkg-open! mod-b)
  (hel-pkg-import! mod-a "" add1)
  (call add2 1)
  (message "%s" (add1 1))
  (hel-pkg-export! add2)
  (hel-pkg-close!))
