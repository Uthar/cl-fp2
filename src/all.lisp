(uiop:define-package cl-fp/all
  (:mix
   #:cl-fp/api
   #:cl-fp/cons
   #:cl-fp/lazy
   #:cl-fp/common
   #:cl-fp/lib
   #:cl)
  (:use-reexport
   #:cl-fp/api
   #:cl-fp/cons
   #:cl-fp/lazy
   #:cl-fp/common
   #:cl-fp/lib))
