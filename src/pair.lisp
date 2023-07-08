(defpackage cl-fp/pair
  (:use :cl)
  (:import-from :murmurhash)
  (:shadowing-import-from :cl-fp/cons :list)
  (:local-nicknames
   (:api :cl-fp/api)
   (:egal :cl-fp/egal))
  (:export
   #:pair
   #:key
   #:val))

(in-package cl-fp/pair)

(defclass pair ()
  ((key :initarg :key :reader get-key)
   (value :initarg :value :reader get-value))
  (:documentation "Key-value pair."))

(defun pair (key value)
  (make-instance 'pair :key key :value value))

(defmethod api:first ((pair pair))
  (get-key pair))

(defmethod api:rest ((pair pair))
  (list (get-value pair)))

(defun key (pair)
  (get-key pair))

(defun val (pair)
  (get-value pair))

(defmethod murmurhash:murmurhash ((pair pair) &key)
  (murmurhash:murmurhash (list (key pair) (val pair))))

(defmethod egal:egal ((x pair) (y pair))
  (and (egal:egal (key x) (key y))
       (egal:egal (val x) (val y))))


