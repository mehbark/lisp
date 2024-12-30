(ql:quickload "flexi-streams")

(defpackage :mehpng
  (:use :common-lisp :flexi-streams))
(in-package :mehpng)
(load "utils.lisp")

(defmacro hexbytes (&rest bytes)
  (map '(vector flex:octet) (λ (x) (reread "#x" x)) bytes))

(defparameter signature
  (hexbytes 89 50 4E 47 0D 0A 1A 0A))

(defgeneric bmatch (obj stream)
  (:documentation "attempt to match OBJ with the binary input stream STREAM.
returns (VALUES BOOLEAN T)"))

(defmethod bmatch ((obj vector) stream)
  (values
   (block nil
     (loop for exp across signature
           for got = (read-byte stream nil nil)
           do (unless (and got (= exp got))
                (return)))
     t)
   nil))
