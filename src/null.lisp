(defpackage cl-fp/null
  (:use :cl)
  (:local-nicknames
   (:api :cl-fp/api)))

(in-package cl-fp/null)

(defmethod api:first ((coll null))
  nil)
