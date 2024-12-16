(load "utils.lisp")

(defun unpole (xs)
  (if (inq (car xs) + - * / ^)
      (mvbind (lhs rest) (unpole (cdr xs))
        (mvbind (rhs rest) (unpole rest)
          (values (list (car xs) lhs rhs) rest)))
      (values (car xs) (cdr xs))))
