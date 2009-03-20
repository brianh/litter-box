(ns litterbox.matrix-simple
  (:use litterbox.utils)
  (:import (java.util Arrays)))

(comment
  (load-file "C:\\home\\lisp\\clj\\src\\git-repos\\litterbox\\matrix-simple.clj")
)

(defstruct matrix ::nrows ::ncols ::data)

(defn make-matrix
  ( [numrows numcols data]
      (struct numrows numcols matrix data)))

(defn get-minor [matrix i]
  (let [{:keys [ncols data]} matrix 
	cnt (count data)]
    (apply vector (map (partial nth data) (range i cnt ncols)))))

(defn get-major [matrix i]
  (let [{:keys [ncols data]} matrix 
	start (* i ncols)]
    (subvec data start (+ start ncols))))

(defn get-entry
  ( [matrix i]
      (nth (:data matrix) i))
  ( [matrix i j]
      (nth (:data matrix) (+ (* i (:ncols matrix)) j))))

; uses modulo arithmatic... 
(defn transpose [{rs :nrows cs :ncols data :data :as m}]
  (let [cnt (count data)
	iter-rng (- cnt 1)]
    (conj (apply vector (map (partial nth data) 
			     (take iter-rng (iterate (fn [x] (mod (+ x cs) iter-rng)) 0))))
	  (nth data iter-rng))))

(defn mult [m1 m2]
  (make-matrix (:nrows m1) (:ncols m2)
	       (doseq [r (:data m1)
		       c (:data m2)]
		 (reduce + (map * r c)))))

(defn prn-matrix [m]
  (print (apply str (map (partial apply prn-str) (:nrows m)))))

(comment
  (def m1 (sqr-matrix 50 (repeatedly (partial rand-int 2))))
  (def m2 (sqr-matrix 50 (repeatedly (partial rand-int 2))))
  (def m1 (sqr-matrix 256 (rands) into-array))
  (def m2 (sqr-matrix 256 (rands) into-array))
  (def m3 (am-mult-hinted m1 m2))
  (time (do (doall (:data m3)) nil))
  (time (reduce unchecked-add (map unchecked-multiply (range 100) (repeat 1))))
  (time (reduce + (map * (range 100) (repeat 1))))
  
  (def m1 (sqr-matrix 5 (repeatedly (partial rand-int 10)) into-array))

  (def a (apply vector (range 1 101)))
  (def b (double-array 100 (repeatedly (partial rand-int 5))))
  (def m1 (make-matrix 10 10 a))
)
