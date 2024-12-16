(load "utils.lisp")

(defun to-lisp (term)
  (match term
    ((list '& vars body)
     (let1 vars (mklist vars)
       `(lambda ,vars
          (declare (type function ,@vars)
                   (ignorable ,@vars)
                   (optimize (speed 3)
                             ;; we know the code is safe
                             (safety 0)))
          ,(to-lisp body))))
    ((list f x) `(funcall ,(to-lisp f) ,(to-lisp x)))
    ((list* f x1 xs) (to-lisp `((,f ,x1) ,@xs)))
    (x x)))
