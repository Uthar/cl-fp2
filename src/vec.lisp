(defpackage cl-fp/vec
  (:use :cl)
  (:shadow :vector)
  (:import-from :rb-vector)
  (:import-from :murmurhash)
  (:local-nicknames
   (:api :cl-fp/api)
   (:egal :cl-fp/egal)
   (:util :cl-fp/util))
  (:export
   #:vector))

(in-package cl-fp/vec)

(defun vector (&rest elems)
  (apply #'rb-vector:rb-vector elems))

(defmethod api:conj ((vec rb-vector:rb-vector) val)
  (rb-vector:append vec val))

(defmethod api:count ((vec rb-vector:rb-vector))
  (rb-vector:count vec))

(defmethod api:empty ((vec rb-vector:rb-vector))
  (rb-vector:rb-vector))

(defmethod api:first ((vec rb-vector:rb-vector))
  (rb-vector:lookup vec 0))

(defmethod api:rest ((vec rb-vector:rb-vector))
  (rb-vector:slice vec 1))

(defmethod api:next  ((vec rb-vector:rb-vector))
  (let ((rest (api:rest vec)))
    (when (plusp (api:count rest))
      rest)))

(defmethod api:seq ((vec rb-vector:rb-vector))
  (when (api:first vec)
    vec))

(defmethod api:assoc ((vec rb-vector:rb-vector) key val &rest keyvals)
  (assert (evenp (length keyvals)))
  (cl:reduce (lambda (vec keyval)
               (destructuring-bind (key val) keyval
                 (rb-vector:insert vec key val)))
             (util:partition 2 keyvals)
             :initial-value (rb-vector:insert vec key val)))

(defmethod api:get ((vec rb-vector:rb-vector) key &optional default)
  (check-type key (integer 0))
  (if (< key (api:count vec))
      (rb-vector:lookup vec key)
      default))

(defmethod api:contains? ((vec rb-vector:rb-vector) key)
  (check-type key (integer 0))
  (< key (api:count vec)))

(defmethod api:nth ((vec rb-vector:rb-vector) index &optional default)
  (check-type index (integer 0))
  (if (< index (api:count vec))
      (rb-vector:lookup vec index)
      (or default (error "Index out of bounds"))))

(defmethod api:pop ((vec rb-vector:rb-vector))
  (rb-vector:pop vec))

(defmethod api:peek ((vec rb-vector:rb-vector))
  (rb-vector:peek vec))

(defmethod print-object ((vec rb-vector:rb-vector) stream)
  (format stream "[")
  (let ((vec-size (rb-vector:count vec))
        (position 0))
    (dotimes (index vec-size)
      (format stream "~s" (rb-vector:lookup vec index))
      (unless (= (incf position) vec-size)
        (format stream " "))))
  (format stream "]"))

(defmethod murmurhash:murmurhash ((object rb-vector:rb-vector) &key)
  (let ((count (rb-vector:count object)))
    (do ((list (cl:list))
         (index 0 (1+ index)))
        ((= index count) (murmurhash:murmurhash list))
      (push (rb-vector:lookup object index) list))))

(defmethod egal:egal ((x rb-vector:rb-vector) (y rb-vector:rb-vector))
  (let ((x-count (rb-vector:count x))
        (y-count (rb-vector:count y)))
    (and (egal:egal x-count y-count)
         (loop for x-index below x-count
               for y-index below y-count
               always (egal:egal (rb-vector:lookup x x-index)
                                 (rb-vector:lookup y y-index))))))
