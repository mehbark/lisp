(load "utils.lisp")

(defvar *eps* 0.00000001d0)

(defun deriv (f)
  (fpromote f
    (lambda (x)
      (/ (- (f (+ x *eps*)) (f x))
         *eps*))))

(defun diff>eps (a b)
  (> (abs (- a b)) *eps*))

(defun solve-newton (f x0)
  (let ((fp (deriv f)))
    (fpromote (f fp)
      (nlet iter ((xn x0))
        (let ((xn+1 (- xn (/ (f  xn)
                             (fp xn)))))
          (if (diff>eps xn xn+1)
              (iter xn+1)
              xn+1))))))

(defvar *max-iterations* 1000)

(defun solve-bisection (f min max)
  (fpromote f
    (assert (not (= (signum (f min)) (signum (f max)))))
    (let ((n 0))
      (loop while (< n *max-iterations*) do
        (let ((mid (/ (+ min max) 2.0d0)))
          (when (or (< (abs (f mid)) *eps*)
                    (< (/ (- max min) 2) *eps*))
              (return mid))
          (if (= (signum (f min)) (signum (f mid)))
              (setf min mid)
              (setf max mid))
          (incf n))))))

(defun solve-secant (f x0 x1)
  (fpromote f
    (nlet iter ((x0 x0) (x1 x1) (n 0))
      (let ((x2 (- x1 (/ (* (f x1) (- x1 x0)) (- (f x1) (f x0))))))
        (if (and (< n *max-iterations*)
                 (diff>eps x1 x2)
                 (diff>eps (f x2) 0))
            (iter x1 x2 (+ n 1))
            x2)))))
