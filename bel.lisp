(load "utils.lisp")

(defun string->chars (s)
  (declare (type string s))
  (map 'list #'identity s))

(defun chars->string (cs)
  (map 'string #'identity cs))

(defparameter prims
  `((id    . ,#'eq)
    (ins   . ,*terminal-io*)
    (outs  . ,*terminal-io*)
    (join  . ,#'(lambda (&optional a b) (cons a b)))
    (car   . ,#'car)
    (cdr   . ,#'cdr)
    (apply . ,#'(lambda (f &rest args) (apply #'call f args)))
    (type  . ,#'(lambda (x)
                (typecase x
                  (symbol 'symbol)
                  (cons 'pair)
                  (character 'char)
                  (stream 'stream))))
    (sym  . ,(fn (intern chars->string)))
    (nom  . ,(fn (string->chars symbol-name)))
    (coin . ,#'(lambda () (zerop (random 2))))
    ;; todo
    (sys  . ,(fn (uiop:run-program chars->string)))))

(defclass lit ()
  ((tag :initarg :tag)
   (stuff :initarg :stuff)))

(defmethod print-object ((obj lit) stream)
  (with-slots (tag stuff) obj
    (format stream "#lit#~s#~s" tag stuff)))

(defmacro lit (tag &rest stuff)
  `(make-instance 'lit :tag ',tag :stuff ',stuff))

(defun call (f &rest args)
  (format t "~s ~s~%" f args)
  (typecase f
    (function (apply f args))
    (lit
     (with-slots (tag stuff) f
       (case tag
         (prim (apply (cdr (assoc (car stuff) prims))
                      args)))))))

(defmacro bel-if (&rest branches)
  (if branches
      `(cond
         ,(loop for (a b . rest) on branches by #'cddr
                collecting
                (if (or b rest)
                    `(,a ,b)
                    `(t ,a))))
      nil))

;; remember: compile!
(defun bel-compile (expr)
  (match expr
    ((or 't 'nil 'o
         (type number)
         (list 'quote _))
     expr)

    ((type string) `',(string->chars expr))

    ((list 'xar place car) `(setf (car ,(bel-compile place)) ,(bel-compile car)))
    ((list 'xdr place cdr) `(setf (cdr ,(bel-compile place)) ,(bel-compile cdr)))

    ((list* 'lit tag stuff) (make-instance 'lit :tag tag :stuff stuff))

    ((list* 'if branches)
     (let1 branches (mapcar #'bel-compile branches)
       `(bel-if ,@branches)))

    ((list* f args) `(call ,(bel-compile f) ,@(mapcar #'bel-compile args)))

    ((type symbol)
     (aif (assoc expr prims)
          `(lit prim ,(car it))
          expr))))
