(load "utils.lisp")
; i've done this before

(deftype cell () '(mod 256))

; TODO: group ops
(defun comp-part (&optional (port *standard-input*))
  (begin
    (loop with out
          for c = (read-char port nil nil)
          while (and c (not (eql c #\])))
          when (in c #\+ #\- #\> #\< #\. #\, #\[)
            do (push
                (ecase c
                  (#\+ '(inc))
                  (#\- '(dec))
                  (#\> '(minc))
                  (#\< '(mdec))
                  ; TODO: fix
                  (#\. '(put-byte))
                  (#\, '(get-byte))
                  (#\[ `(loop until (zerop cell) do (progn ,@(comp-part port)))))
                out)
          finally (return (nreverse out)))))

(defun comp (&optional (port *standard-input*))
  `(begin
     (declare (optimize (speed 3) (safety 0)))
     (= mem (make-array '(65536) :element-type 'cell))
     (= mp 0)
     (=sm cell (aref mem mp))

     (=f inc (&optional (n 1)) (setf cell (mod (+ cell n) 256)))
     (=f dec () (inc -1))

     (=f minc (&optional (n 1)) (setf mp (mod (+ mp n) 65536)))
     (=f mdec () (minc -1))

     (=f get-byte () (setf cell (mod (char-code (read-char *standard-input* nil #\null)) 256)))
     (=f put-byte () (write-char (code-char cell)))

     (=m bf-loop (&body body)
         `(loop until (zerop cell)
                do (progn ,@body)))
     ,@(comp-part port)
     t))
