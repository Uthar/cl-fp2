(defpackage cl-fp/vec
  (:use :cl)
  (:shadow :vector)
  (:import-from :rb-vector)
  (:local-nicknames
   (:api :cl-fp/api))
  (:export
   #:vector))

(in-package cl-fp/vec)

(defun vector (&rest elems)
  (apply #'rb-vector:rb-vector elems))

(defmethod api:conj ((vec rb-vector:rb-vector) val)
  (rb-vector:append vec val))

(defmethod api:seq ((vec rb-vector:rb-vector))
  vec)

(defmethod api:count ((vec rb-vector:rb-vector))
  (rb-vector:count vec))

(defmethod api:empty ((vec rb-vector:rb-vector))
  (rb-vector:rb-vector))

(defmethod api:first ((vec rb-vector:rb-vector))
  (rb-vector:lookup vec 0))

(defmethod api:rest ((vec rb-vector:rb-vector))
  (rb-vector:slice vec 1))

;; (defmethod api:assoc ((vec rb-vector:rb-vector) key val &rest keyvals)
