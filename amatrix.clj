(ns 'amatrix)

(comment
  (load-file "C:\\home\\lisp\\clj\\src\\git-repos\\litterbox\\amatrix.clj")
)

(def b (into-array (map double-array (partition 10 (take 100 (repeatedly (partial rand-int 2)))))))
(def m1 (make-matrix 10 10 int-array 0))

(defstruct matrix :nrows :ncols :data)

(defn make-matrix
  ( [numrows numcols afn def-val]
      (make-matrix
       (into-array (map afn (partition numrows (repeat (* numrows numcols) def-val))))))
  ( [data]
      (struct matrix (count data) (count (aget data 0)) data)))

(defn transpose [{rs :nrows cs :ncols data :data :as m}]
  (let [cnt (count data)
	iter-rng (- cnt 1)]
    (conj (apply vector (map (partial nth data) 
			     (take iter-rng (iterate (fn [x] (mod (+ x cs) iter-rng)) 0))))
	  (nth data iter-rng))))

(defn prn-array [a]
  (str (map str a)))
  
(defn prn-matrix [m]
  (loop [nrows (::nrows m)
	 