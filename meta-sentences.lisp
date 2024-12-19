(load "utils.lisp")

(defun main ()
  (loop with seen
        with fmt = (read-line)
        for len = 10 then (length sentence)
        for sentence = (format nil fmt len)
        until (member len seen)
        do (nunionf seen (list len))
        do (format t "~3d ~a~%" (length sentence) sentence)))
