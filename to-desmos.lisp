(load "utils.lisp")

(defgeneric to-desmos (obj port)
  (:documentation "write OBJ in desmos-flavored latex to PORT"))

(defmethod to-desmos ((obj complex) port)
  (to-desmos (realpart obj) port)
  (write-char #\+ port)
  (to-desmos (imagpart obj) port)
  (write-char #\i port))

(defmethod to-desmos ((obj real) port)
  (format port "~f" obj))

(defmethod to-desmos ((obj rational) port)
  (format port "\\frac{")
  (to-desmos (numerator obj) port)
  (format port "}{")
  (to-desmos (denominator obj) port)
  (format port "}"))

(defmethod to-desmos ((obj integer) port)
  (format port "~d" obj))

(defmethod to-desmos ((obj list) port)
  (format port "\\left[")
  (loop for (n . rest) on obj
        do (to-desmos n port)
        when rest do (format port ","))
  (format port "\\right]"))
