(defpackage cl-fp/lazy
  (:use :cl)
  (:local-nicknames
   (:p :lparallel)
   (:api :cl-fp/api)
   (:cons :cl-fp/cons))
  (:export
   :lazy-seq))

(in-package cl-fp/lazy)

(defclass lazy-seq (api:collection)
  ((delay :initarg :delay :reader get-delay)))

(defmacro lazy-seq (seq-form)
  `(make-instance 'lazy-seq :delay (p:delay ,seq-form)))

(defmethod api:conj ((lazy-seq lazy-seq) val)
  (cons:cons val lazy-seq))

(defmethod api:empty ((lazy-seq lazy-seq))
  (cons:list))

(defmethod api:first ((lazy-seq lazy-seq))
  (api:first (p:force (get-delay lazy-seq))))

(defmethod api:rest ((lazy-seq lazy-seq))
  (api:rest (p:force (get-delay lazy-seq))))
