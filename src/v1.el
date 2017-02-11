(require 'dash)

;; {{ UTILS }}

(defalias 'set! 'setq)
  
(defmacro error-unless (cond-expr fmt-str &rest fmt-args)
  (declare (indent defun))
  `(unless ,cond-expr
     (error ,fmt-str ,@fmt-args)))

;; {{ TYPES }}

;; Ref is symbol reference that has additional link to its source package
(defun hel-ref (pkg sym) (vector pkg sym))
(defun hel-ref-pkg (ref) (aref ref 0))
(defun hel-ref-sym (ref) (aref ref 1))

;; {{ GLOBAL STATE }}

(defconst hel-pkg nil)
(defconst hel-pkg-symbols (make-hash-table :test #'eq))
(defconst hel-pkg-map (make-hash-table :test #'eq))

;; {{ CORE DEFS }}

(defun hel-intern (module sym)
  (intern (format "<%s.%s>" module sym)))

;; {{ PACKAGE-RELATED }}

(defmacro hel-pkg-open! (pkg)
  (when hel-pkg
    (error "Already has opened package `%s'. Close it" hel-pkg))
  (set! hel-pkg pkg)
  nil)

(defun hel-pkg-close! ()
  (unless hel-pkg
    (error "No package is opened to be closed"))
  (set! hel-pkg nil)
  (clrhash hel-pkg-symbols))

(defmacro hel-pkg-provide! (&rest symbols)
  (error-unless symbols "Can not provide a package without any symbols")
  (dolist (sym symbols)
    (error-unless (hel--pkg-contains? hel-pkg sym)
      "Symbol `%s' does not belong to opened package" sym))
  (puthash hel-pkg symbols hel-pkg-map)
  nil)

(defun hel--pkg-put-sym! (src-pkg prefix sym)
  (puthash (intern (concat prefix (symbol-name sym)))
           (hel-intern src-pkg sym)
           hel-pkg-symbols))

(defun hel--pkg-contains? (pkg sym)
  (fboundp (hel-intern pkg sym)))

(defmacro hel-pkg-require! (src-pkg prefix &rest symbols)
  (let ((src-pkg-symbols (gethash src-pkg hel-pkg-map)))
    (unless src-pkg-symbols
      (error "Package `%s' not found" src-pkg))
    (if symbols
        ;; Get only listed symbols
        (dolist (sym symbols)
          (unless (memq sym src-pkg-symbols)
            (error (if (hel--pkg-contains? src-pkg sym)
                       "Symbol `%s' is private inside package `%s'"
                     "Symbol `%s' not found inside package `%s'")
                   sym
                   src-pkg))
          (hel--pkg-put-sym! src-pkg prefix sym))
      ;; Get all package public symbols
      (dolist (sym symbols) (hel--pkg-put-sym! src-pkg prefix sym))))
  nil)

(defun hel--pkg-define! (def sym params forms)
  (error-unless hel-pkg
    "No package is opened, can not define symbol `%s'" sym)
  (let ((priv-name (hel-intern hel-pkg sym)))
    (puthash sym priv-name hel-pkg-symbols)
    `(,def ,priv-name ,params ,@forms)))

(defmacro hel-pkg-defun! (sym params &rest forms)
  (hel--pkg-define! 'defun sym params forms))

(defmacro hel-pkg-defmacro! (sym params &rest forms)
  (hel--pkg-define! 'defmacro sym params forms))

;; {{ SANDBOX }}

(defmacro call (f &rest args)
  (cons (gethash f hel-pkg-symbols f) args))

(defmacro quote-all (&rest forms) (declare (indent defun)))

(quote-all
  (hel-pkg-open! mod-a)
  (hel-pkg-defun! add1 (x) (+ 1 x))
  (hel-pkg-defun! add2 (x) (call add1 (call add1 x)))
  (hel-pkg-defmacro! macros (x) (list 'quote (cons (cdr x) (car x))))
  (hel-pkg-provide! add2)
  (hel-pkg-close!)
  
  ;; defined `mod-a.add1'

  (hel-pkg-open! mod-b)
  (hel-pkg-require! mod-a "" add2)
  (call add2 1)
  (message "%s" (add1 1))
  (hel-pkg-close!))
