(load "utils.lisp")

(defparameter example
  '(
    (class (Monoid a)
     (mzero a)
     (mplus (-> a a a)))

    (instance (Monoid String)
     (mzero "")
     (mplus (lambda (a b) (native (concatenate 'string a b)))))

    (def + (-> Number Number Number)
     (lambda (a b) (native (+ a b))))

    (instance (Monoid Number)
     (mzero 0)
     (mplus +))

    (def msum (=> (Monoid a) (-> (List a) a))
     (lambda (xs)
       (List.iter xs
                  mzero
                  mplus)))
    ))
