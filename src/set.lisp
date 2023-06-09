(defpackage cl-fp/set
  (:use :cl)
  (:import-from :cl-hamt)
  (:import-from :alexandria :if-let)
  (:import-from :murmurhash)
  (:local-nicknames
   (:cons :cl-fp/cons)
   (:egal :cl-fp/egal)
   (:api :cl-fp/api))
  (:export
   #:hash-set))

(in-package cl-fp/set)

(defun hash-set (&rest elems)
  (apply #'cl-hamt:set-insert (cl-hamt:empty-set :test #'egal:egal) elems))
   
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
  (cl-hamt:empty-set :test #'egal:egal))

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
   
(defmethod print-object ((set cl-hamt:hash-set) stream)
  (format stream "#{")
  (let ((set-size (cl-hamt:set-size set))
        (position 0))
    (dolist (entry (cl-hamt:set->list set))
      (format stream "~s" entry)
      (unless (= (incf position) set-size)
        (format stream " "))))
  (format stream "}"))

(defmethod murmurhash:murmurhash ((object cl-hamt:hash-set) &key)
  (murmurhash:murmurhash (cl-hamt:set->list object)))

(defmethod egal:egal ((x cl-hamt:hash-set) (y cl-hamt:hash-set))
  (let ((x-count (cl-hamt:set-size x))
        (y-count (cl-hamt:set-size y)))
    (and (egal:egal x-count y-count)
         (cl-hamt:set-eq x y))))
