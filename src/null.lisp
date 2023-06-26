(defpackage cl-fp/null
  (:use :cl)
  (:local-nicknames
   (:api :cl-fp/api)
   (:cons :cl-fp/cons)))

(in-package cl-fp/null)

(defmethod api:first ((coll null))
  nil)

(defmethod api:rest ((coll null))
  cons::+empty-list+)

(defmethod api:next ((coll null))
  nil)
