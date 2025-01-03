;;; fun
(proclaim '(inline last1 single append1 conc1))
;; TODO: packagify and such

;; has a lot of goodies
;; hey whaddayaknow, pg has a with-gensyms
;; it's just such a useful pattern
;; maybe i should go wild with the defmacro!?g stuff from LOL

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("alexandria" "trivia") :silent t)
  (use-package '(:alexandria :trivia))

  (proclaim '(inline mklist))

  (defun mklist (obj)
    (if (listp obj) obj (list obj)))

  (defmacro let1 (name val &body body)
    `(let ((,name ,val))
       ,@body))

  ;; labels should rarely be needed
  (defmacro fpromote ((&rest fs) &body body)
    `(labels ,(loop for f in (mklist fs)
                    collecting
                    (let ((fname
                            (if (listp f)
                                (car f)
                                f))
                          (fval
                            (if (listp f)
                                (cadr f)
                                f))
                          (xs (gensym "args")))
                      `(,fname (&rest ,xs) (apply ,fval ,xs))))
       ,@body))

  (defmacro nlet (name (&rest vars) &body body)
    (let1 vars (mapcar #'mklist vars)
      `(labels ((,name ,(mapcar #'car vars) ,@body))
         (,name ,@(mapcar #'cadr vars)))))

  (defun group (source n)
    (if (zerop n) (error "zero length"))
    (labels ((rec (source acc)
               (let1 rest (nthcdr n source)
                 (if (consp rest)
                     (rec rest (cons (subseq source 0 n) acc))
                     (nreverse (cons source acc))))))
      (if source (rec source nil) nil)))

  (defmacro cond1 (&body branches)
    `(cond ,@(mapcar (lambda (x)
                       (if (cdr x)
                           x
                           (cons t x)))
                     (group branches 2))))
  (defmacro when-bind ((var expr) &body body)
    `(let ((,var ,expr))
       (when ,var
         ,@body)))

  (defmacro when-bind* (binds &body body)
    (if (null binds)
        `(progn ,@body)
        `(let (,(car binds))
           (if ,(caar binds)
               (when-bind* ,(cdr binds) ,@body)))))

  (defun condlet-binds (vars cl)
    (mapcar #'(lambda (bindform)
                (if (consp bindform)
                    (cons (cdr (assoc (car bindform) vars))
                          (cdr bindform))))
            (cdr cl)))

  (defun condlet-clause (vars cl bodfn)
    `(,(car cl) (let ,(mapcar #'cdr vars)
                  (let ,(condlet-binds vars cl)
                    (,bodfn ,@(mapcar #'cdr vars))))))

  ;; straight up do not get this one
  #+(or)
  (defmacro condlet (clauses &body body)
    (let ((bodfn (gensym))
          (vars (mapcar #'(lambda (v) (cons v (gensym)))
                        (remove-duplicates
                         (mapcar #'car
                                 (mappend #'cdr clauses))))))
      `(labels ((,bodfn ,(mapcar #'car vars)
                  ,@body))
         (cond ,@ (mapcar #'(lambda (cl)
                              (condlet-clause vars cl bodfn))
                          clauses))))))

(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (f lst)
  (fpromote f
    (let (acc)
      (dolist (x lst)
        (let1 val (f x)
          (if val (push val acc))))
      (nreverse acc))))

(defun prune (test tree)
  (fpromote test
    (labels ((rec (tree acc)
               (cond1
                 (null tree) (nreverse acc)
                 (consp (car tree)) (rec (cdr tree)
                                         (cons (rec (car tree) nil) acc))
                 (rec (cdr tree)
                      (if (test (car tree))
                          acc
                          (cons (car tree) acc))))))
      (rec tree nil))))

(defun find2 (f lst)
  (fpromote f
    (if (null lst)
        nil
        (let1 val (f (car lst))
          (if val
              (values (car lst) val)
              (find2 f (cdr lst)))))))

(defun before (x y lst &key (test #'eql))
  (fpromote test
    (and lst
         (let1 first (car lst)
           (cond1
             (test y first) nil
             (test x first) lst
             (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  (let1 rest (before y x lst :test test)
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (f lst)
  (fpromote f
    (let (acc)
      (do ((src lst (cdr src)))
          ((or (null src) (f (car src)))
           (values (nreverse acc) src))
        (push (car src) acc)))))

(defun most (f lst)
  (fpromote f
    (if (null lst)
        (values nil nil)
        (let* ((wins (car lst))
               (max  (f wins)))
          (dolist (obj (cdr lst))
            (let1 score (f obj)
              (when (> score max)
                (setf wins obj
                      max  score))))
          (values wins max)))))

(defun best (f lst)
  (fpromote f
    (if (null lst)
        nil
        (let1 wins (car lst)
          (dolist (obj (cdr lst))
            (if (f obj wins)
                (setf wins obj)))
          wins))))

(defun mostn (f lst)
  (fpromote f
    (if (null lst)
        (values nil nil)
        (let ((result (list (car lst)))
              (max (f (car lst))))
          (dolist (obj (cdr lst))
            (let1 score (f obj)
              (cond1
                (> score max) (setf max    score
                                    result (list obj))
                (= score max) (push obj result))))
          (values (nreverse result) max)))))

(defun mapa-b (f a b &optional (step 1))
  (fpromote f
    (do ((i a (+ i step))
         (result nil))
        ((> i b) (nreverse result))
      (push (f i) result))))

(defun map0-n (f n)
  (mapa-b f 0 n))

(defun map1-n (f n)
  (mapa-b f 1 n))

(defun map-> (f start test succ)
  (fpromote (f test succ)
    (do ((i start (succ i))
         (result nil))
        ((test i) (nreverse result))
      (push (f i) result))))

(defun mapcars (f &rest lsts)
  (fpromote f
    (let (result)
      (dolist (lst lsts)
        (dolist (obj lst)
          (push (f obj) result)))
      (nreverse result))))

(defun rmapcar (f &rest args)
  (if (some #'atom args)
      (apply f args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar f args))
             args)))

(defun readlist (&rest args)
  (values (read-from-string
           (concatenate 'string
                        "("
                        (apply #'read-line args)
                        ")"))))

(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (f quit &rest args)
  (format *query-io* "Entering break-loop ~%")
  (loop
    (let1 in (apply #'prompt args)
      (if (funcall quit in)
          (return)
          (format *query-io* "~a~%" (funcall f in))))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

;; likely undesirable
(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))

;; do we want this?
;; (defvar *!equivs* (make-hash-table))
;;
;; (defun ! (fn)
;;   (or (gethash fn *!equivs*) fn))
;;
;; (defun def! (fn fn!)
;;   (setf (gethash fn *!equivs*) fn!))
;;
;; (def! #'remove-if #'delete-if)

(defun memoize (f)
  (let1 cache (make-hash-table :test #'equal)
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply f args)))))))

;; yes
;; (defun compose (&rest fs)
;;   (if fs
;;       (let ((f1 (last1 fs))
;;             (fs (butlast fs)))
;;         #'(lambda (&rest args)
;;             (reduce #'funcall fs
;;                     :from-end t
;;                     :initial-value (apply f1 args))))
;;       #'identity))

;; maybe should take &rest args?
(defun fif (ifp then &optional else)
  (fpromote (ifp then else)
    #'(lambda (x)
        (if (ifp x)
            (then x)
            (if else (else x))))))

;; f-intersection
(defun fint (f &rest fs)
  (if (null fs)
      f
      (let1 chain (apply #'fint fs)
        (fpromote (f chain)
          #'(lambda (x)
              (and (f x) (chain x)))))))

;; f-union
(defun fun (f &rest fs)
  (if (null fs)
      f
      (let1 chain (apply #'fun fs)
        (fpromote (f chain)
          #'(lambda (x)
              (or (f x) (chain x)))))))

(defun lrec (rec &optional base)
  (fpromote (rec base)
    (labels ((self (lst)
               (if (null lst)
                   (if (functionp base)
                       (base)
                       base)
                   (rec (car lst)
                        #'(lambda ()
                            (self (cdr lst)))))))
      #'self)))

(defun ttrav (rec &optional (base #'identity))
  (fpromote (rec base)
    (labels ((self (tree)
               (if (atom tree)
                   (if (functionp base)
                       (base tree)
                       base)
                   (rec (self (car tree))
                        (if (cdr tree)
                            (self (cdr tree)))))))
      #'self)))

(defun trec (rec &optional (base #'identity))
  (fpromote (rec base)
    (labels
        ((self (tree)
           (if (atom tree)
               (if (functionp base)
                   (base tree)
                   base)
               (rec tree
                    #'(lambda () (self (car tree)))
                    #'(lambda ()
                        (if (cdr tree)
                            (self (cdr tree))))))))
      #'self)))

(defmacro nil! (&rest vars)
  `(setf ,@(loop for var in vars
                 nconcing (list var nil))))

(defmacro nif (n &optional pos zer neg)
  (once-only (n)
    `(cond
       ((plusp  ,n) ,pos)
       ((minusp ,n) ,neg)
       (t ,zer))))

;; sly already pretty-prints soooo
(defmacro mac (expr)
  (let1 expanded (gensym "EXPANDED")
    `(let ((,expanded (macroexpand-1 ',expr)))
       ;; (pprint ,expanded)
       ,expanded)))

(defmacro in (obj &rest choices)
  (once-only (obj)
    `(or ,@(mapcar #'(lambda (c) `(eql ,obj ,c))
                   choices))))

(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar #'(lambda (a) `',a)
                      args)))

(defmacro in-if (fn &rest choices)
  (once-only (fn)
    `(or ,@ (mapcar #'(lambda (c) `(funcall ,fn ,c))
                    choices))))

(defun >casex (g cl)
  (let ((key (car cl)) (rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
          ((inq key t otherwise) `(t ,@rest))
          (t (error "bad >case clause")))))

(defmacro >case (expr &rest clauses)
  (once-only (expr)
    `(cond ,@(mapcar #'(lambda (cl) (>casex expr cl))
                     clauses))))

;; we have loop for for while and till

(defmacro allf (val &rest args)
  (once-only (val)
    `(setf ,@(mapcan #'(lambda (a) `(,a ,val))
                     args))))

(defmacro nilf (&rest args)
  `(allf nil ,@args))

(define-modify-macro toggle1 () not)
(defmacro toggle (&rest args)
  `(progn
     ,@(mapcar #'(lambda (a) `(toggle1 ,a))
               args)))

;; more from alexandria
;; concf (nconcf)
;; concnew (roughly, unionf)

;; very cool
(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
                       (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))

;; ANAPHORS!
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &body body)
  `(aif ,test
        (progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

; improvements inspired by SBCL https://github.com/sbcl/sbcl/blob/42d939ea06063bcddd29d628a4ba6613edf4bcf8/src/code/primordial-extensions.lisp#L321 (should probably destructure too :P)
(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (once-only ((test (caar clauses)))
        `(if ,test
             ,(aif (cdar clauses)
                   `(let ((it ,test))
                      (declare (ignorable it))
                      ,@it)
                   test)
             (acond ,@(cdr clauses))))))

(defmacro alambda (args &body body)
  `(labels ((self ,args ,@body))
     #'self))

;; reminds me of clojure's -> (i'll add that why not)
;; (i'll probably end up integrating another alexandria-like that has it anyway :P)
;; (n)let me be fancy
(defmacro ablock (tag &rest args)
  `(block ,tag
     ,(nlet self ((args args))
        (case (length args)
          (0 nil)
          (1 (car args))
          (t `(let1 ((it ,(car args)))
                ,(self (cdr args))))))))

;; clever!
;; is the or necessary?
(defmacro aif2 (test then &optional else)
  (with-gensyms (ok)
    `(multiple-value-bind (it ,ok) ,test
       (if (or it ,ok)
           ,then
           ,else))))

(defmacro awhen2 (test &body body)
  `(aif2 ,test
         (progn ,@body)))

(defmacro awhile2 (test &body body)
  (with-gensyms (flag)
    `(let ((,flag t))
       (loop while ,flag
             (aif2 ,test
                   (progn ,@body)
                   (setf ,flag nil))))))

;; doubt i'll use it but for completeness
(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses)))
        (with-gensyms (val win)
          `(multiple-value-bind (,val ,win) ,(car cl1)
             (if (or ,val ,win)
                 (let ((it ,val)) ,@(cdr cl1))
                 (acond2 ,@(cdr clauses))))))))

;; i was being foolish
;; pg's is awesome stuff
#|
(defvar *fn-builders* (make-hash-table))
(defmacro defbuilder (name (&rest args) &body body)
  `(setf (gethash ',name *fn-builders*) (alambda ,args ,@body)))

(defbuilder lambda (args &rest body) `(lambda ,args ,@body))
(defbuilder and (&rest fs)
  (with-gensyms (args)
    `(lambda (&rest ,args)
       (and ,@(loop for f in fs
                    collecting `(apply #',(rbuild f) ,args))))))
(defbuilder or (&rest fs)
  (with-gensyms (args)
    `(lambda (&rest ,args)
       (or ,@(loop for f in fs
                   collecting `(apply #',(rbuild f) ,args))))))
(defbuilder compose (&rest fs)
  )
|#

(declaim (ftype (function (t) t) rbuild))

(defun build-compose (fns)
  (with-gensyms (g)
    `(lambda (,g)
       ,(nlet rec ((fns fns))
          (if fns
              `(,(rbuild (car fns))
                ,(rec (cdr fns)))
              g)))))

(defun build-call (op fns)
  (with-gensyms (g)
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                          `(,(rbuild f) ,g))
                      fns)))))

(defun rbuild (expr)
  (if (or (atom expr) (inq (car expr) lambda function))
      expr
      (if (eq (car expr) 'compose)
          (build-compose (cdr expr))
          (build-call (car expr) (cdr expr)))))

(defmacro fn (expr)
  "shorthand for writing functions of a single argument
(fn f)                 => #'f
(fn (f g))             => (lambda (x) (f (g x)))
(fn (compose a b c d)) => (lambda (x) (a (b (c (d x)))))
(fn (list f g h))      => (lambda (x) (list (f x) (g x) (h x)))
"
  `#',(rbuild expr))

(defmacro alrec (rec &optional base)
  (with-gensyms (fn)
    `(lrec #'(lambda (it ,fn)
               (symbol-macrolet ((rec (funcall ,fn)))
                 ,rec))
           ,base)))

(defmacro on-cdrs (rec base &rest lsts)
  `(funcall (alrec ,rec #'(lambda () ,base)) ,@lsts))

(defmacro atrec (rec &optional (base 'it))
  (with-gensyms (lfn rfn)
    `(trec #'(lambda (it ,lfn ,rfn)
               (declare (ignorable it))
               (symbol-macrolet ((left (funcall ,lfn))
                                 (right (funcall ,rfn)))
                 ,rec))
           #'(lambda (it) ,base))))

(defmacro on-trees (rec base &rest trees)
  `(funcall (atrec ,rec ,base) ,@trees))

(defvar unforced (gensym))

;; a struct instead of a lambda is more visible
(defstruct promise forced closure)

;; interesting approach
(defmacro delay (&body body)
  (with-gensyms (self)
    `(let ((,self (make-promise :forced unforced)))
       (setf (promise-closure ,self)
             #'(lambda ()
                 (setf (promise-forced ,self) (progn ,@body))))
       ,self)))

(defun force (x)
  (if (promise-p x)
      (if (eq (promise-forced x) unforced)
          (funcall (promise-closure x))
          (promise-forced x))
      x))

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defmacro abbrevs (&rest names)
  `(progn
     ,@(loop for pair in (group names 2)
             collecting `(abbrev ,@pair))))

(abbrevs dbind  destructuring-bind
         mvbind multiple-value-bind
         mvsetq multiple-value-setq
         λ      lambda)

;; not great
(defmacro propmacro (propname)
  `(defmacro ,propname (obj)
     `(get ,obj ',',propname)))

(defmacro propmacros (&rest props)
  `(progn
     ,@(loop for prop in props
             collecting `(propmacro ,prop))))

;; meh
;; i bet this'll come up
(defun fix (f x0 &key (test #'eql))
  (fpromote (f test)
    (nlet wow ((x (f x0)) (last x0))
      (if (test x last)
          x
          (wow (f x) x)))))

(defmacro as-> (val name &body body)
  (cond1
    (null body) val
    (null (cdr body)) `(let ((,name ,val)) ,(car body))
    `(let* ,(mapcar #'(lambda (b) `(,name ,b)) (cons val (butlast body)))
       ,(lastcar body))))

;; even more for fun
(defmacro a-> (val &body body)
  `(as-> ,val it ,@body))

;; (defmacro <>-> (val &body body)
;;   `(as-> ,val <> ,@body))

;; the value of this is that (-> x car) is (car x) instead of (let1 it x (car x)), enabling setf
(defmacro -> (x &body body)
  (match body
    ((cons (cons f xs) rest)
     `(-> (,f ,x ,@xs) ,@rest))
    ((cons f rest)
     `(-> (,f ,x) ,@rest))
    (nil x)))

(defmacro doprod ((&rest binds) &body body)
  (if (null binds)
      `(progn ,@body)
      `(loop for ,(first (car binds)) in ,(second (car binds))
             do (doprod ,(cdr binds) ,@body))))

(defmacro mapprod ((&rest binds) &body body)
  (if (null binds)
      `(progn ,@body)
      `(loop for ,(first (car binds)) in ,(second (car binds))
             collect (mapprod ,(cdr binds) ,@body))))

(defpattern structure (expr)
  (match expr
    ((list 'quote x) `',x)
    ((cons a b) `(cons (structure ,a) (structure ,b)))
    (x x)))

(defmacro begin (&body body)
  "PROGN that admits internal, sequential defines written = a la arc"
  (match body
    ((list stmt) stmt)
    ((cons stmt rest)
     (match stmt
       ((structure ('= ('values . binds) . body))
        `(multiple-value-match (progn ,@body)
           (,binds (begin ,@rest))))

       ((list* '= pattern body)
        `(let-match1 ,pattern (begin ,@body)
           (begin ,@rest)))

       ((structure ('=f name args . body))
        `(labels ((,name ,args (begin ,@body)))
           (begin ,@rest)))

       ((structure ('=m name args . body))
        `(macrolet ((,name ,args (begin ,@body)))
           (begin ,@rest)))

       ((structure ('=sm sym val))
        `(symbol-macrolet ((,sym ,val))
           (begin ,@rest)))

       (_ `(locally ,stmt (begin ,@rest)))))
    (_ nil)))

(defmacro l$ (&body body)
  (begin
    (=f $? (sym) (and (symbolp sym) (starts-with #\$ (symbol-name sym))))
    (=f $-value (sym) (or (parse-integer (string-trim "$" (symbol-name sym))
                                         :junk-allowed t)
                          -1))

    (= money (remove-duplicates (remove-if-not #'$? (flatten body))))
    (= highest (apply #'max (cons 0 (mapcar #'$-value money))))

    `(lambda (,@(loop for i from 1 to highest
                      collecting (symb '$ i))
              ,@(when (member '$@ money)
                  '(&rest $@)))
       ,@body)))
