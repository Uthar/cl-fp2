(defpackage cl-fp/set
  (:use :cl)
  (:import-from :cl-hamt)
  (:import-from :alexandria :if-let)
  (:local-nicknames
   (:cons :cl-fp/cons)
   (:api :cl-fp/api)))

(in-package cl-fp/set)

(defun hash-set (&rest elems)
  (apply #'cl-hamt:set-insert (cl-hamt:empty-set) elems))
   
(defmethod api:conj ((set cl-hamt:hash-set) val)
  (cl-hamt:set-insert set val))

(defmethod api:seq ((set cl-hamt:hash-set))
  (cl-hamt:set-reduce
   (lambda (list val)
     (cons:cons val list))
   set
   (cons:list)))

(defmethod api:count ((set cl-hamt:hash-set))
  (cl-hamt:set-size set))

(defmethod api:empty ((set cl-hamt:hash-set))
  (cl-hamt:empty-set))

(defmethod api:get ((set cl-hamt:hash-set) key &optional default)
  (if-let ((found (cl-hamt:set-lookup set key)))
    key
    default))

(defmethod api:contains? ((set cl-hamt:hash-set) key)
  (cl-hamt:set-lookup set key))

(defmethod api:disj ((set cl-hamt:hash-set) key &rest keys)
  (cl:reduce
   (lambda (set key)
     (cl-hamt:set-remove set key))
   keys
   :initial-value set))
   
