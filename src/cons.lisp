(defpackage cl-fp/cons
  (:use :cl)
  (:shadow :cons :list)
  (:local-nicknames
   (:egal :cl-fp/egal)
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

(defclass empty-list (cons) ())
(defparameter +empty-list+ (make-instance 'empty-list))
(defmethod api:first ((coll (eql +empty-list+))) nil)
(defmethod api:rest  ((coll (eql +empty-list+))) +empty-list+)
(defmethod api:empty ((coll (eql +empty-list+))) +empty-list+)
(defmethod api:seq   ((coll (eql +empty-list+))) +empty-list+)
(defmethod api:next  ((coll (eql +empty-list+))) nil)
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

(defmethod api:next  ((cons cons))
  (let ((rest (api:rest cons)))
    (unless (eq rest +empty-list+)
      rest)))
        
(defmethod api:count ((cons cons))
  (do ((count 0 (1+ count))
       (next cons (api:next next)))
      ((null next) count)))

(defmethod api:conj  ((cons cons) x) (cons x cons))

(defmethod egal:egal ((x cons) (y cons))
    (loop for x2 = x then (api:next x2)
          for y2 = y then (api:next y2)
          while (or x2 y2)
          always (egal:egal (api:first x2)
                            (api:first y2))))
        
