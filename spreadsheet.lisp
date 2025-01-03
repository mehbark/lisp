(load "utils.lisp")

;; n-dimensional, why not
(defclass sheet ()
  ((cells :initarg :cells :type (simple-array function))))

;; todo: identify dependencies and lazily recompute
;; (i would use promises, but)
;; having everything be a thunk is advantageous
(defmacro sheet (&body rows)
  (begin
    (= width (length (car rows)))
    (assert (apply #'= (mapcar #'length rows)))
    (= cells-name (gensym "CELLS"))
    (= current-index (gensym "CURRENT-INDEX"))
    (= cells
       `(list
         ,@(loop for row in rows
               for y from 0
               collecting
               `(list
                 ,@(loop for cell in row
                         for x from 0
                         collecting `(lambda () (let1 ,current-index '(,x ,y) ,cell)))))))
    (= cells
       `(progn
          (defvar ,current-index nil)
          (labels ((,cells-name ()
                     (make-array '(,(length rows) ,width)
                                 :element-type 'function
                                 :initial-contents ,cells))
                   (cell (x y)
                     (assert (not (equal (list x y) ,current-index)))
                     (funcall (aref (,cells-name) y x))))
            (,cells-name))))
    cells))
