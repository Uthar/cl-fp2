(defpackage cl-fp/cons
  (:use :cl)
  (:shadow :cons :list)
  (:local-nicknames
   (:api :cl-fp/api))
  (:export
   #:cons
   #:list))

(in-package cl-fp/cons)

(defclass cons (api:collection)
  ((first :initarg :first :reader get-first)
   (rest :initarg :rest :reader get-rest)))

(defun cons (x coll)
  (check-type coll api:collection)
  (make-instance 'cons
                 :first x
                 :rest coll))

(defclass empty-list (api:collection) ())
(defparameter +empty-list+ (make-instance 'empty-list))
(defmethod api:first ((coll (eql +empty-list+))) nil)
(defmethod api:rest  ((coll (eql +empty-list+))) +empty-list+)
(defmethod api:empty ((coll (eql +empty-list+))) +empty-list+)
(defmethod api:seq   ((coll (eql +empty-list+))) +empty-list+)
(defmethod api:count ((coll (eql +empty-list+))) 0)
(defmethod api:conj  ((coll (eql +empty-list+)) x) (cons x +empty-list+))

(defun list (&rest xs)
  (reduce (lambda (cons x)
            (cons x cons))
          (reverse xs)
          :initial-value +empty-list+))

(defmethod api:first ((cons cons)) (get-first cons))
(defmethod api:rest  ((cons cons)) (or (get-rest cons) +empty-list+))
(defmethod api:empty ((cons cons)) +empty-list+)
(defmethod api:seq   ((cons cons)) cons)
(defmethod api:count ((cons cons))
  (do ((count 0 (1+ count))
       (rest cons (api:rest rest)))
      ((eq rest +empty-list+) count)))
(defmethod api:conj  ((cons cons) x) (cons x cons))


