(load "utils.lisp")

(defun first-n-bits (n int)
  (loop for i below n
        collecting (logbitp i int)))

(defun sat (expr)
  (let* ((vars (delete-duplicates
                (remove-if (lambda (x) (inq x and or not xor t nil))
                           (flatten expr))))
         (varc (length vars))
         (f (eval `(lambda ,vars
                     (declare (type boolean ,@vars) (optimize (speed 3) (safety 0) (debug 0)))
                     ,expr))))
    (disassemble f)
    (loop for n below (expt 2 varc)
          do (let1 bits (first-n-bits varc n)
               (when (apply f bits)
                 (return-from sat
                   (mapcar #'list vars bits)))))
    :unsat))

(defmacro time-s (expr)
  (with-gensyms (start end diff)
    `(begin
       (= ,start (get-internal-real-time))
       ,expr
       (= ,end (get-internal-real-time))
       (= ,diff (- ,end ,start))
       (/ ,diff internal-time-units-per-second))))

(defparameter timings
  (loop for var-count below 30
        collecting
        (time-s (sat `(and ,@(loop for i below var-count
                                   collecting (symb 'v i)))))))
