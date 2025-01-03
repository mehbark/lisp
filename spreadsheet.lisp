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
    (= cells `(list ,@(mapcar )))
    (= cells `(make-array (list width (length rows))
                          :element-type 'function
                          :initial-contents
                          (macrolet ((cell (&rest subscripts) `(aref ,cells-name ,@subscripts)))
                            ,cells)))
    ))

(sheet
 (1 2 3 4 5)
 (6 7 8 9 10))
