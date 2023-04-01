(defpackage cl-fp/lazy
  (:use :cl)
  (:local-nicknames
   (:p :lparallel)
   (:fp :cl-fp/api)
   (:cons :cl-fp/cons)))

(in-package cl-fp/lazy)

(defclass lazy-seq (fp:collection)
  ((delay :initarg :delay :reader get-delay)))

(defmacro lazy-seq (seq-form)
  `(make-instance 'lazy-seq :delay (p:delay ,seq-form)))

(defmethod fp:conj ((lazy-seq lazy-seq) val)
  (cons:cons val lazy-seq))

(defmethod fp:empty ((lazy-seq lazy-seq))
  (cons:list))

(defmethod fp:first ((lazy-seq lazy-seq))
  (fp:first (p:force (get-delay lazy-seq))))

(defmethod fp:rest ((lazy-seq lazy-seq))
  (fp:rest (p:force (get-delay lazy-seq))))
