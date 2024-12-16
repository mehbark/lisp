(load "utils.lisp")

(defmacro defadt (name &rest clauses)
  `(prog1
     (defclass ,name () ())
     ,@(loop for clause in clauses
             nconcing
             (begin
               (= (cons clause-name slots) (mklist clause))
               (= keywords (mapcar (lambda (x) (intern (string x) :keyword)) slots))
               `((defclass ,clause-name (,name)
                   ,(loop for slot in slots
                          for keyword in keywords
                          collecting `(,name :initarg ,keyword)))
                 ,(if slots
                      `(defun ,clause-name ,slots
                         (make-instance ',clause-name
                                        ,@(loop for slot in slots
                                                for keyword in keywords
                                                nconcing
                                                `(,keyword ,slot))))
                      `(defparameter ,clause-name
                         (make-instance ',clause-name))))))))

;; aaaaand match just works :P

(defadt NonEmpty (End x) (NCons hd tl))

(defun nlength (x)
  (declare (type NonEmpty x))
  (match x
    ((End (x _)) 1)
    ((NCons (hd _) tl) (+ 1 (nlength tl)))))
