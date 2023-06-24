(defpackage cl-fp/map
  (:use :cl)
  (:import-from :cl-hamt)
  (:shadowing-import-from
   #:cl-fp/cons
   #:list
   #:cons)
  (:import-from :cl-fp/pair :pair)
  (:local-nicknames
   (:lib :cl-fp/lib)
   (:api :cl-fp/api))
  (:export
   #:hash-map))

(in-package cl-fp/map)

(defun hash-map (&rest keyvals)
  (apply #'cl-hamt:dict-insert (cl-hamt:empty-dict) keyvals))

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
                           
          
           
