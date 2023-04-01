(defpackage cl-fp/cons
  (:use :cl)
  (:shadow :cons :list)
  (:local-nicknames
   (:fp :cl-fp/api))
  (:export
   #:cons
   #:list))

(in-package cl-fp/cons)

(defclass cons (fp:collection)
  ((first :initarg :first :reader get-first)
   (rest :initarg :rest :reader get-rest)))

(defun cons (x coll)
  (check-type coll fp:collection)
  (make-instance 'cons
                 :first x
                 :rest coll))

(defclass empty-list (fp:collection) ())
(defparameter +empty-list+ (make-instance 'empty-list))
(defmethod fp:first ((coll (eql +empty-list+))) nil)
(defmethod fp:rest  ((coll (eql +empty-list+))) +empty-list+)
(defmethod fp:empty ((coll (eql +empty-list+))) +empty-list+)
(defmethod fp:conj  ((coll (eql +empty-list+)) x) (cons x +empty-list+))

(defun list (&rest xs)
  (reduce (lambda (cons x)
            (cons x cons))
          (reverse xs)
          :initial-value +empty-list+))

(defmethod fp:first ((cons cons)) (get-first cons))
(defmethod fp:rest  ((cons cons)) (or (get-rest cons) +empty-list+))
(defmethod fp:empty ((cons cons)) +empty-list+)
(defmethod fp:conj  ((cons cons) x) (cons x cons))


