(load "utils.lisp")

(defmacro fstr (str)
  (begin
    (= cmd nil)
    (= outstr (gensym "outstr"))
    (=f emit (c) (push `(princ ,c ,outstr) cmd))
    (with-input-from-string (s str)
      (loop with escaping? = nil
            for c = (read-char s nil nil)
            while c
            do (cond
                 (escaping? (emit c)
                            (setf escaping? nil))
                 (t (match c
                      (#\\ (setf escaping? t))
                      (#\{
                       (begin
                         (= chars
                            (loop for c = (read-char s nil nil)
                                  while c
                                  until (eql c #\})
                                  collecting c))
                         (= str (map 'string #'identity chars))
                         (emit (read-from-string str))))
                      (_   (emit c)))))))
    `(with-output-to-string (,outstr)
       ,@(nreverse cmd))))

