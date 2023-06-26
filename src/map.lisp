(defpackage cl-fp/map
  (:use :cl)
  (:import-from :cl-hamt)
  (:import-from :murmurhash)
  (:shadowing-import-from
   #:cl-fp/cons
   #:list
   #:cons)
  (:import-from :cl-fp/pair :pair)
  (:local-nicknames
   (:egal :cl-fp/egal)
   (:api :cl-fp/api))
  (:export
   #:hash-map))

(in-package cl-fp/map)

(defun hash-map (&rest keyvals)
  (apply #'cl-hamt:dict-insert (cl-hamt:empty-dict :test #'egal:egal) keyvals))

(defmethod api:conj ((coll cl-hamt:hash-dict) x)
  (cl-hamt:dict-insert coll (api:first x) (api:first (api:rest x))))
  
(defmethod api:seq ((map cl-hamt:hash-dict))
  (unless (zerop (cl-hamt:dict-size map))
    (let ((alist (cl-hamt:dict->alist map)))
      (cl:reduce (lambda (pairs cons)
                   (destructuring-bind (key . value) cons
                     (cons (make-instance 'pair :key key :value value)
                           pairs)))
                 alist
                 :initial-value (list)))))

(defmethod api:count ((map cl-hamt:hash-dict))
  (cl-hamt:dict-size map))

(defmethod api:empty ((map cl-hamt:hash-dict))
  (cl-hamt:empty-dict :test #'egal:egal))

(defmethod api:assoc ((map cl-hamt:hash-dict) key val &rest keyvals)
  (assert (evenp (length keyvals)))
  (apply #'cl-hamt:dict-insert map key val keyvals))

(defmethod api:dissoc ((map cl-hamt:hash-dict) key &rest keys)
  (apply #'cl-hamt:dict-remove map key keys))

(defmethod api:get ((map cl-hamt:hash-dict) key &optional default)
  (multiple-value-bind (value found)
      (cl-hamt:dict-lookup map key)
    (if found
        value
        default)))

(defmethod api:contains? ((map cl-hamt:hash-dict) key)
  (multiple-value-bind (value found)
      (cl-hamt:dict-lookup map key)
    (declare (ignore value))
    found))

(defmethod print-object ((map cl-hamt:hash-dict) stream)
  (format stream "{")
  (let ((map-size (cl-hamt:dict-size map))
        (position 0))
    (dolist (entry (cl-hamt:dict->alist map))
      (destructuring-bind (key . value) entry
        (format stream "~s ~s" key value)
        (unless (= (incf position) map-size)
          (format stream ", ")))))
  (format stream "}"))
               
(defmethod murmurhash:murmurhash ((object cl-hamt:hash-dict) &key)
  (murmurhash:murmurhash (cl-hamt:dict->alist object)))

(defmethod egal:egal ((x cl-hamt:hash-dict) (y cl-hamt:hash-dict))
  (let ((x-count (cl-hamt:dict-size x))
        (y-count (cl-hamt:dict-size y)))
    (and (egal:egal x-count y-count)
         (cl-hamt:dict-eq x y))))
