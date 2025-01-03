(ql:quickload "flexi-streams")

(defpackage :mehpng
  (:use :common-lisp :flexi-streams))
(in-package :mehpng)
(load "utils.lisp")

(defmacro hexbytes (&rest bytes)
  (map '(vector flex:octet) (λ (x) (reread "#x" x)) bytes))

(defgeneric bmatch (obj stream)
  (:documentation "attempt to match OBJ with the binary input stream STREAM.
returns (VALUES BOOLEAN T)"))

(defmacro bor (&body opts)
  (with-gensyms (ok? val)
    (match opts
      ((list a b)
       `(mvbind (,ok? ,val) ,a
          (if ,ok? ,val ,b)))
      ((cons a b*)
       `(bor ,a (bor ,@b*)))
      (nil '(values nil nil)))))

(defmacro band (&body opts)
  (with-gensyms (a-ok? a-val b-ok? b-val)
    (match opts
      ((cons a b*)
       `(mvbind (,a-ok? ,a-val) ,a
                (if ,a-ok?
                    (mvbind (,b-ok? ,b-val) (band ,@b*)
                       (if ,b-ok?
                           (values t (cons ,a-val ,b-val))
                           (bor)))
                    (bor))))
      (nil '(values t nil)))))

(defparameter signature
  (hexbytes 89 50 4E 47 0D 0A 1A 0A))

(defmethod bmatch ((obj vector) stream)
  (values 
   (loop for exp across signature
         for got = (read-byte stream nil nil)
         do (unless (and got (= exp got))
              (return))
         finally (return t)) 
   nil))
