(load "utils.lisp")

(defclass ?v ()
  ((constraints :initform nil)))

(defclass constraint () ())
(defclass c= (constraint)
  ((n :initarg :n :type number)))

;; constr should be generic and return bool
(defvar failure (gensym))
(defun failure? (sym) (eq sym failure))

(defmacro definverse ((?fw ?bw) op)
  (with-gensyms (a b a-op-b)
   `(progn
      (defgeneric ,?fw (,a ,b ,a-op-b))
      (defgeneric ,?bw (,a ,b ,a-op-b))

      (defmethod ,?bw (,a ,b ,a-op-b)
        (,?fw ,b ,a-op-b ,a))

      (defmethod ,?fw ((,a number) (,b number) (,a-op-b number))
        (if (= (,op ,a ,b) ,a-op-b)
            nil
            failure))

      (defmethod ,?fw ((,a number) (,b number) (,a-op-b symbol))
        (list (list '= (,op ,a ,b) ,a-op-b)))

      (defmethod ,?fw ((,a number) (,b symbol) (,a-op-b number))
        (,?bw )))))

(definverse (?+ ?-) +)
