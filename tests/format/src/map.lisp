(defun (map f list)
 (if (null list)
  nil
  (cons (f (car list)) (map f (cdr list)))))
