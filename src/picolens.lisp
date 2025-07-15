(in-package :picolens)

;; fmap for Const
(defun const (f x)
  (declare (ignore f))
  x)

(declaim (inline over))
(defun over (lens f x)
  "Modify the target of a lens applying @c(f) to it."
  (declare (optimize (speed 3))
           (type function lens f))
  (funcall lens #'funcall f x))

(declaim (inline set))
(defun set (lens val x)
  "Replace the target of a lens with @c(val)."
  (declare (optimize (speed 3)))
  (over lens (constantly val) x))

(declaim (inline view))
(defun view (lens x)
  "Get the target of a lens, a getter or a fold."
  (declare (optimize (speed 3))
           (type function lens))
  (funcall lens #'const #'identity x))

(defun empty (fmap f x)
  "Neutral element in monoid of lenses under composition:

@begin[lang=lisp](code)
(equalp (view empty x) x)               ; => T
(equalp (over empty f x) (funcall f x)) ; => T
@end(code)"
  (funcall fmap #'identity (funcall f x)))

(defun compose (&rest ls)
  "Compose lenses.

For example
@begin[lang=lisp](code)
(view (compose (to #'cl:cdr) (to #'cl:car)) x)
@end(code)

is equal to
@begin[lang=lisp](code)
(cl:car (cl:cdr x))
@end(code)"
  (declare (optimize (speed 3)))
  (reduce
   (lambda (l1 l2)
     (declare (type function l1 l2))
     (lambda (fmap f x)
       (declare (type function fmap f))
       (funcall
        l1 fmap
        (lambda (y) (funcall l2 fmap f y))
        x)))
   ls))

(defun lens (get set)
  "Make a lens out of a setter of type @c(a -> b -> c) and a getter of
type @c(b -> a).

Example:
@begin[lang=lisp](code)
(lens #'cl:car (lambda (v cons) (cons v (cl:cdr cons))))
@end(code)
is a lens equivalent of @c(cl:car)."
  (declare (optimize (speed 3))
           (type function get set))
  (lambda (fmap f x)
    (declare (type function fmap f))
    (funcall
     fmap (lambda (s)
            (funcall set s x))
     (funcall f (funcall get x)))))

(defun to (get)
  "Make a getter out of a function @c(get). An attempt to use this
getter with @c(over) or @c(set) results in a runtime error."
  (declare (optimize (speed 3))
           (type function get))
  (lens
   get (lambda (s x)
         (declare (ignore s x))
         (error "Cannot act like setter"))))

(defun car (fmap f x)
  "Lens equivalent of @c(cl:car)"
  (funcall
   fmap (lambda (s)
          (cons s (cl:cdr x)))
   (funcall f (cl:car x))))

(defun cdr (fmap f x)
  "Lens equivalent of @c(cl:cdr)"
  (funcall
   fmap (lambda (s)
          (cons (cl:car x) s))
   (funcall f (cl:cdr x))))

(defun cadr (fmap f x)
  "Lens equivalent of @c(cl:cadr)"
  (funcall
   fmap (lambda (s)
          (cons (cl:car x) (cons s (cl:cddr x))))
   (funcall f (cl:cadr x))))

(defun caar (fmap f x)
  "Lens equivalent of @c(cl:caar)"
  (funcall
   fmap (lambda (s)
          (cons (cons s (cl:cdar x)) (cl:cdr x)))
   (funcall f (cl:caar x))))

(defun cddr (fmap f x)
  "Lens equivalent of @c(cl:cddr)"
  (funcall
   fmap (lambda (s)
          (cons (cl:car x) (cons (cl:cadr x) s)))
   (funcall f (cl:cddr x))))

(defun cdar (fmap f x)
  "Lens equivalent of @c(cl:cdar)"
  (funcall
   fmap (lambda (s)
          (cons (cons (cl:caar x) s) (cl:cdr x)))
   (funcall f (cl:cdar x))))

(defun all (fmap f x)
  "Lens which targets all elements of a list"
  (funcall fmap #'identity (mapcar f x)))

(defun fold (f &rest gs)
  "Make a fold out of multiple getters @c(gs) and an associative
binary operation @c(f):

@begin[lang=lisp](code)
(view (fold #'* #'picolens:car #'picolens:cdr) '(2 . 3)) ; => 6
@end(code)"
  (declare (optimize (speed 3))
           (type function f))
  (reduce
   (lambda (g1 g2)
     (to
      (lambda (x)
        (funcall f (view g1 x) (view g2 x)))))
   gs))

(defmacro gen-lenses (constructor &body lens-specs)
  "Generate lenses for a container (e.g. a structure, a class or even
a cons cell). This macro is called with @c(constructor) which is the
name of a constructor of the container followed by lens specification
in the form @c((reader-name lens-name)) for each slot (in the order
determined by the constructor). Here is an example:

@begin[lang=lisp](code)
(serapeum:defconstructor node
  (value integer)
  (left  (or null node))
  (right (or null node)))

(gen-lenses node
  (node-value value)
  (node-left  left)
  (node-right right))
@end(code)"
  `(progn
     ,@(loop for spec in lens-specs
             for i from 0 by 1
             collect
             (destructuring-bind (reader lens) spec
               `(defun ,lens (fmap f x)
                  (declare (optimize (speed 3))
                           (type function fmap f))
                  (funcall
                   fmap (lambda (s)
                          (,constructor
                           ,@(loop for spec in lens-specs
                                   for reader = (first spec)
                                   for j from 0 by 1
                                   collect
                                   (if (= i j) `s `(,reader x)))))
                   (funcall f (,reader x))))))))
