(defun vec:find (vec pred &optional begin end)
  (let ((pos (or begin 0))
        (count (or end (length vec)))
        (result nil))
    (while (< pos count)
      (if (funcall pred (aref vec pos))
          (progn
            (setq result (aref vec pos))
            (setq pos count))
        (setq pos (1+ pos))))
    result))

(defun vec:contains? (vec x &optional begin end)
  (let ((pos (or begin 0))
        (count (or end (length vec)))
        (result nil))
    (while (< pos count)
      (if (equal x (aref vec pos))
          (progn
            (setq result t)
            (setq pos count))
        (setq pos (1+ pos))))
    result))

(defmacro dovector (var-vec &rest forms)
  (declare (indent defun))
  (let ((var (nth 0 var-vec))
        (vec (nth 1 var-vec)))
    `(let (,var
           (pos 0)
           (count (length ,vec)))
       (while (< pos count)
         (setq ,var (aref ,vec pos))
         ,@forms
         (setq pos (1+ pos))))))

