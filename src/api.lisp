(defpackage cl-fp/api
  (:use :cl)
  (:shadow
   #:count
   #:=
   #:first
   #:rest
   #:assoc
   #:nth
   #:get
   #:pop
   #:subseq)
  (:export
   #:collection
   #:conj
   #:seq
   #:count
   #:empty
   #:=
   #:first
   #:rest
   #:next
   #:assoc
   #:dissoc
   #:get
   #:contains?
   #:nth
   #:pop
   #:peek
   #:disj
   #:rseq
   #:subseq
   #:rsubseq))

(in-package cl-fp/api)

(defclass collection () ())

;; Collection
(defgeneric conj (coll val))
(defgeneric seq (coll))
(defgeneric count (coll))
(defgeneric empty (coll))
(defgeneric = (x y))

;; Sequence
(defgeneric seq (coll))
(defgeneric first (coll))
(defgeneric rest (coll))
(defgeneric next (coll))

;; Associative
(defgeneric assoc (map key val))
(defgeneric dissoc (map key))
(defgeneric get (map key &optional default))
(defgeneric contains? (coll key))

;; Indexed
(defgeneric nth (coll index))

;; Stack
(defgeneric pop (coll))
(defgeneric peek (coll))

;; Set
(defgeneric disj (set key))

;; Sorted
(defgeneric rseq (rev))
(defgeneric subseq (coll test key))
(defgeneric rsubseq (coll test key))

