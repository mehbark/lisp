(defstruct node
  data
  (l nil :type (or null node))
  (r nil :type (or null node)))

(defun node (data &optional l r)
  (make-node :data data :l l :r r))

;;     1
;;    / \
;;   2   3
;;  / \ 
;; 4   5
(defparameter example
  (node 1
    (node 2
      (node 4)
      (node 5))
    (node 3)))

(defun maplonger (f alist blist)
  (loop for a = alist then (cdr a)
        for b = blist then (cdr b)
        while (or (car a) (car b))
        collecting (funcall f (car a) (car b))))

(defun levels (node)
  (if node
      (with-slots (data l r) node
        (cons (list data)
              (maplonger #'nconc (levels l) (levels r))))
      nil))

(defun print-levels (node)
  (loop for level in (levels node)
        do (format t "~{~a~^ ~}~%" level)))
