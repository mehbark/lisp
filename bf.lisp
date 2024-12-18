(load "utils.lisp")

(defmacro incmodf (place mod &optional (delta 1))
  `(setf ,place (mod (+ ,place ,delta) ,mod)))

; TODO: ooo could totally gen from sym (e.g. '++-[]>)
; then i'd need some sort of var syntax ugh
(defpattern cmd (op val)
  `(list ',op ,val))

; mandelbrot is about 5kb shorter with this :P
; zeroing-add gets us more
(defun optimize-loop (ins)
  (match ins
    ((list (cmd inc _)) '(setf cell 0))
    ((guard
      (list (cmd inc -1)
            (cmd minc rmove)
            (cmd inc  1)
            (cmd minc lmove))
      (= lmove (- rmove))) ; [->{n}+<{n}]
     `(zeroing-add ,rmove))
    ; maybe warn here?
    ((list) '(unless (zerop cell) (loop)))
    (_ nil)))

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
                 (#\[ (put)
                  (let1 inner (comp-part port)
                    (push (or (optimize-loop inner)
                              `(loop until (zerop cell)
                                     do (progn ,@inner)))
                          out))))
          finally (return (nreverse out)))))

(defparameter mem-size 32768)

(defun comp (&optional (port *standard-input*))
  (eval
   `(lambda (&key (in *standard-input*) (out *standard-output*))
      (declare (optimize (speed 3) (safety 0)))
      (begin
        (= mem (make-array ,mem-size :element-type '(unsigned-byte 8)))
        (= mp 0)
        (=sm cell (aref mem mp))

        (=f inc  (n) (incmodf cell 256 n))
        (=f minc (n) (incmodf mp ,mem-size n))

        ; TODO: fix
        (=f get-byte () (setf cell (mod (char-code (read-char in nil #\null)) 256)))
        (=f put-byte () (write-char (code-char cell) out))

        (=f zeroing-add (rmove)
            (incmodf (aref mem (mod (+ mp rmove) ,mem-size)) 256 cell)
            (setf cell 0))

        ,@(comp-part port)

        t))))

(defun main ()
  (funcall (comp)))
