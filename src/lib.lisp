(defpackage cl-fp/lib
  (:use :cl)
  (:shadow :reduce)
  (:shadowing-import-from :cl-fp/api :collection :first :rest :seq)
  (:shadowing-import-from :cl-fp/cons :cons :list)
  (:shadowing-import-from :cl-fp/lazy :lazy-seq)
  (:export
   #:range
   #:take
   #:drop
   #:iterate
   #:reduce))

(in-package cl-fp/lib)

(defun range (&key (start 0) end (step 1))
  "Return an infinite sequence of natural numbers from start to end"
  (cond
    ((null end)
     (lazy-seq (cons start (range :start (+ start step)
                                  :step step))))
    ((>= start end)
     (list))
    (t
     (lazy-seq (cons start (range :start (+ start step)
                                  :end end
                                  :step step))))))


(defun take (n coll)
  "Returns a lazy sequence of the first n items in coll"
  (if (<= n 0)
      (lazy-seq (list))
      (lazy-seq (cons (first coll) (take (1- n) (rest coll))))))

(defun drop (n coll)
  "Returns a lazy sequence of all but the first n items in coll"
  (if (plusp n)
      (drop (1- n) (rest coll))
      (lazy-seq coll)))

(defun iterate (f x)
  "Return an infinite sequence of applications of f to the previous
   iteration's value"
  (cons x (lazy-seq (iterate f (funcall f x)))))

(defun reduce (f coll &key init)
  (labels
      ((lp (acc cur)
         (if (not (seq cur))
             acc
             (lp (funcall f acc (first cur))
                 (rest cur)))))
    (if init
        (lp init coll)
        (lp (first coll) (rest coll)))))
