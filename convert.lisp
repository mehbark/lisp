(load "utils.lisp")
(ql:quickload "fset")
(named-readtables:in-readtable fset:fset-readtable)

;; TODO: compound
;; let's do the form :cm :cm :cm first

(defparameter conv
  '((:kg (1000 :g))
    (:g  (1000 :mg))
    (:m  (100 :cm))
    (:km (1000 :m))
    (:in (254/100 :cm))
    (:ft (12 :in))
    (:yd (3 :ft))
    (:mi (1760 :yd))
    (:ml (1 :cm :cm :cm))
    (:hz (1 (:s -1)))
    (:acre (4840 (:yd 2)))))

(defparameter dims
  '(((:time)   :fs :ps :ns :ms :cs :s :ks :gs :min :hr :day :yr :century :millennium)
    ((:length) :fm :pm :nm :mm :cm :m :km :gm :in :ft :yd :mi)
    ((:mass)   :fg :pg :ng :mg :cg :g :kg :gg :lb)
    ((:length 2) :acre)
    ((:time -1) :hz)))

;; multiset is not sufficient!
;; why did i not read this comment!

(defun dim (&rest dims)
  (let1 out (fset:with-default #{| |} 0)
    (loop for (dim . rest) on dims
          when (keywordp dim)
            do (incf (fset:@ out dim)
                     (if (aand (car rest)
                               (numberp it))
                         (car rest)
                         1)))
    out))

;; maybe should memoize?
;; hash-table would be ffffast
;; eh dw
(defun convs (from)
  (loop for (a (n b)) in conv
        when (eq a from)
          collect `(,n ,b)
        when (eq b from)
          collect `(,(/ n) ,a)))

(defun convert (n from to &optional past)
  (cond1
    (equal from to) n
    (consp from) (cond1
                   (atom to) :idk
                   (longer from to) nil
                   ;; OMG
                   (apply #'*
                          (mapcar (lambda (f to)
                                    (convert n f to (cons from past)))
                                  from to)))
    (let1 convs (remove-if #'(lambda (x) (member (cadr x) past :test #'equal)) (convs from))
      (if convs
          (loop for (mul new-from) in convs
                do (let1 got (convert (* n mul) new-from to (cons from past))
                     (if got (return got)))) 
          nil))))
