;; TODO:

;; an M*N word search has M horiz, N vert, M diag, N diag

(load "utils.lisp")

;; not good
(defun eacharray (f arr)
  (apply #'map-product
         #'(lambda (&rest idxs) (apply f (apply #'aref arr idxs) idxs))
         (mapcar #'iota (array-dimensions arr))))

(defmacro docross ((x y &optional c) arr &body body)
  (once-only (arr)
    `(loop for ,y below (array-dimension ,arr 0)
           do (loop for ,x below (array-dimension ,arr 1)
                    do
                    ,(if c
                         `(symbol-macrolet ((,c (aref ,arr ,y ,x)))
                            (progn ,@body))
                         `(progn ,@body))))))


(defun things (arr words)
  (docross (x y)))
