(defpackage cl-fp/egal
  (:use :cl)
  (:export
   #:egal))

(in-package cl-fp/egal)

(defgeneric egal (x y))

(defmethod egal ((x t) (y t))
  (and (egal (type-of x) (type-of y))
       (eq x y)))

(defmethod egal ((x symbol) (y symbol))
  (eq x y))

(defmethod egal ((x cl:cons) (y cl:cons))
  (eq x y))

(defmethod egal ((x cl:vector) (y cl:vector))
  (eq x y))

(defmethod egal ((x complex) (y complex))
  (and (egal (realpart x) (realpart y))
       (egal (imagpart x) (imagpart y))))

(defmethod egal ((x rational) (y rational))
  (and (egal (numerator x) (numerator y))
       (egal (denominator x) (denominator y))))

(defmethod egal ((x float) (y float))
  (and (= (float-sign x) (float-sign y))
       (= x y)))

(defmethod egal ((x integer) (y integer))
  (= x y))

(defmethod egal ((x bignum) (y bignum))
  (= x y))
