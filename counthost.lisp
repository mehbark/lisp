;; TODO: make this unnecessary for `sbcl --script` invocations
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(load "utils.lisp")

(defun account-exists? (n)
  (begin
    (= (values _ _ status)
       (uiop:run-program
        (format nil "curl --head --fail https://cohost.org/~r" n)
        :ignore-error-status t))
    (zerop status)))

(loop for i below 100
      do (format t "~a @~r~%" (if (account-exists? i) "∃" "∄") i)
      do (sleep 0.1))
