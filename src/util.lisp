(defpackage cl-fp/util
  (:use :cl)
  (:export :partition))

(in-package cl-fp/util)

(defun partition (n list)
  (do ((list list (subseq list n))
       (parts nil (append parts (list (subseq list 0 n)))))
      ((null list) parts)))
