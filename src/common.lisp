(defpackage cl-fp/common
  (:use :cl)
  (:shadowing-import-from :cl-fp/api :collection :first :next :seq :rest))

(in-package cl-fp/common)

(defmethod next ((coll collection))
  (when (seq coll)
    (rest coll)))

(defmethod next ((coll null))
  nil)

(defmethod seq ((coll collection))
  (when (first coll)
    coll))
