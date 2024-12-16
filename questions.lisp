(load "utils.lisp")

(defstruct node contents yes no)

(defvar *nodes* (make-hash-table))

#+(or)
(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
        (make-node :contents conts
                   :yes yes
                   :no no)))

#+(or)
(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
        (if yes
            #'(lambda ()
                (funcall
                 (gethash
                  (if (eq 'yes (prompt "~a~%>> " conts)) yes no)
                  *nodes*)))
            #'(lambda () conts))))

(defnode 'people "Is the person a man?" 'male 'female)
(defnode 'male "Is he living?" 'liveman 'deadman)
(defnode 'deadman "Was he American?" 'us 'them)
(defnode 'us "Is he on a coin?" 'coin 'cidence)
(defnode 'coin "Is the coin a penny?" 'penny 'coins)
(defnode 'penny 'lincoln)

(defun run-node (name)
  (let1 n (gethash name *nodes*)
    (cond1
      (node-yes n) (run-node
                    (if (eq 'yes (prompt "~a~%>> " (node-contents n)))
                        (node-yes n)
                        (node-no  n)))
      (node-contents n))))
