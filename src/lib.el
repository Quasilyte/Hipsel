(defmacro case (cmp expr &rest forms)
  (declare (indent defun))
  (let ((unprocessed (length forms))
        (val (make-symbol "val"))
        arms
        cond-val
        cond-result
        default-arm)
    (while (> unprocessed 1)
      (set! cond-val `(quote (nth 0 forms))
            cond-result (nth 1 forms)
            arms (cons `((,cmp ,cond-val ,val) ,cond-result) arms)
            forms (nthcdr 2 forms)
            unprocessed (- unprocessed 2)))
    (set! default-arm
          (if forms
              (list t (car forms))
            (list t `(error "Uncovered case executed: %s" ,val))))
    (set! arms (nreverse arms))
    `(let ((,val ,expr))
       (cond ,@arms ,default-arm))))
