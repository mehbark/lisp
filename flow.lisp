(load "utils.lisp")

(defclass node ()
  ((next :initarg :next :initform nil :accessor node-next)))

(defclass if-node (node)
  ((expr :initarg :expr)
   (else :initarg :else :initform nil)))

(defclass output-node (node)
  )

(defgeneric run (node))

(defmethod run ((node node))
  (node-next node))

(defmethod run ((node if-node))
  (with-slots (expr next else) node
    (if (eval expr)
        next
        else)))


;; start
;;   |
;;   v
;; x = 10
;;   |
;;   v
;; if x = 0 -Y> end
;;   | ^
;;   N |<-------
;;   v         ^
;; output x    |
;;   |         |
;;   v         |
;; x = x - 1 ->|

