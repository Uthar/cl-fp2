(defpackage cl-fp/lib
  (:use :cl)
  (:shadow :reduce)
  (:shadowing-import-from :cl-fp/api :get :collection :first :rest :seq :assoc)
  (:shadowing-import-from :cl-fp/cons :cons :list)
  (:shadowing-import-from :cl-fp/lazy :lazy-seq)
  (:export
   #:range
   #:take
   #:drop
   #:iterate
   #:reduce
   #:get-in))

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

(defun get-in (m ks)
  (check-type ks cl:cons)
  (do* ((keys ks (cl:rest keys))
        (map (get m (cl:first keys))
             (get map (cl:first keys))))
       ((null (cl:rest keys)) map)))



;; TODO
#|
assoc-in
associative?
butlast
coll?
concat
counted?
distinct
doall
dorun
doseq
drop
drop-last
drop-while
empty
empty?
every-pred
every?
ffirst
filter
filterv
find
first
flatten
for
frequencies
get
+ get-in
group-by
hash-map
hash-set
indexed?
interleave
interpose
into
iterate
iteration
juxt
keep
keep-indexed
last
lazy-seq
list
list*
list?
map
map-entry?
map-indexed
map?
mapcat
mapv
merge
merge-with
next
nfirst
nnext
not-any?
not-empty
not-every?
nth
nthnext
nthrest
partition
partition-all
partition-by
peek
pmap
reduce
reduce-kv
reductions
repeat
repeatedly
rest
reverse
reversible?
rseq
rsubseq
second
select-keys
seq
seq?
seqable?
sequential?
set?
shuffle
some
some?
sort
sort-by
sorted-map
sorted-set
sorted?
subseq
subvec
take
take-last
take-nth
take-while
update
update-in
update-keys
update-vals
val
vals
vector
vector?
zipmap
|#
