(load "utils.lisp")

(defun comp-part (&optional (port *standard-input*))
  (begin
    (= out nil)
    (= op 'inc)
    (= val 0)
    (=f put (&optional (next 'inc))
        (unless (zerop val) (push (list op val) out))
        (setf op next)
        (setf val 0))

    (loop for c = (read-char port nil nil)
          while (and c (not (eql c #\])))
          when (in c #\+ #\- #\> #\< #\. #\, #\[)
            do (ecase c
                 ((#\+ #\-)
                  (unless (eq op 'inc) (put 'inc))
                  (incf val (if (eql c #\+) 1 -1)))

                 ((#\> #\<)
                  (unless (eq op 'minc) (put 'minc))
                  (incf val (if (eql c #\>) 1 -1)))

                 (#\. (put) (push '(put-byte) out))
                 (#\, (put) (push '(get-byte) out))
                 (#\[ (put) (push `(loop until (zerop cell) do (progn ,@(comp-part port))) out)))
          finally (progn
                    (put)
                    (return (nreverse out))))))

(defun comp (&optional (port *standard-input*))
  (eval
   `(lambda (&key (in *standard-input*) (out *standard-output*))
      (begin
        (declare (optimize (speed 3) (safety 0)))
        (= mem (make-array '(65536) :element-type '(unsigned-byte 8)))
        (= mp 0)
        (=sm cell (aref mem mp))

        (=f inc  (&optional (n 1)) (setf cell (mod (+ cell n) 256)))
        (=f minc (&optional (n 1)) (setf mp   (mod (+ mp n) 65536)))

                                        ; TODO: fix
        (=f get-byte () (setf cell (mod (char-code (read-char in nil #\null)) 256)))
        (=f put-byte () (write-char (code-char cell) out))

        (=m bf-loop (&body body)
            `(loop until (zerop cell)
                   do (progn ,@body)))
        ,@(comp-part port)
        t))))
