(ns litterbox.matrix-simple
  (:use litterbox.utils))

(comment
  (load-file "C:\\home\\lisp\\clj\\src\\git-repos\\litterbox\\matrix-simple.clj")
)

(defstruct matrix :nrows :ncols :data)

(defn make-matrix [numrows numcols data]
  (struct matrix numrows numcols (apply vector data)))

(defn get-minor [matrix i]
  (let [{:keys [ncols data]} matrix 
	cnt (count data)]
    (apply vector (map (partial nth data) (range i cnt ncols)))))

(defn get-major [matrix i]
  (let [{:keys [ncols data]} matrix 
	start (* i ncols)]
    (subvec data start (+ start ncols))))

(defn get-majors [m]
  (map (partial get-major m) (range (:nrows m))))

(defn get-entry [matrix i j]
  (nth (:data matrix) (+ (* i (:nrows matrix)) j)))

; uses modulo arithmatic... 
(defn transpose-data [cs data]
  (let [iter-rng (dec (count data))]
    (conj (apply vector (map (partial nth data) 
			     (take iter-rng (iterate (ring-range cs iter-rng) 0))))
	  (nth data iter-rng))))

(defn transpose [{rs :nrows cs :ncols data :data}]
  (make-matrix cs rs (transpose-data cs data)))

(defn add [m1 m2]
  (make-matrix (:nrows m1) (:ncols m1)
	       (map + (:data m1) (:data m2))))

(defn mult [m1 m2]
  (make-matrix (:nrows m1) (:ncols m2)
	       (for [r (get-majors m1)
		     c (get-majors m2)]
		 (reduce + (map * r c)))))

;; 1/3 faster
(defn mult3 [m1 m2]
  (make-matrix (:nrows m1) (:ncols m2)
	       (map (fn [r c] (reduce + (map * r c)))
		    (repeat-each (:ncols m1)
				 (partition (:ncols m1) (:data m1)))
		    (cycle (partition (:ncols m2) (:data m2))))))

;; slower
(defn mult2 [m1 m2]
  (make-matrix (:nrows m1) (:ncols m2)
	       (for [r (partition (:nrows m1) (:data m1))
		     c (partition (:nrows m2) (:data m2))]
		 (reduce + (map * r c)))))

(defn prn-matrix [m]
  (let [{:keys [nrows ncols data]} m]
    (print (apply str (map (partial apply prn-str) (partition ncols data))))))

(comment
  (def size 3)
  (def size 5)
  (def size 50)
  (def size 150)
  (def size 256)
  (def size 512)
  (def size 1024)

  (def rng 2)
  (def rng 10)

  (def num-gen (repeatedly (partial rand-int rng)))
  
  (def m1 (make-matrix size size (take (* size size) (repeatedly (partial rand-int rng)))))
  (def m2 (make-matrix size size (take (* size size) (repeatedly (partial rand-int rng)))))
  
  (time (do (doall (:data m3)) nil))
  (time (reduce unchecked-add (map unchecked-multiply (range 100) (repeat 1))))
  (time (reduce + (map * (range 100) (repeat 1))))
  
  (def m1 (make-matrix 3 5 (range 15)))

  (def a (apply vector (range 1 101)))
  (def b (double-array 100 (repeatedly (partial rand-int 5))))n
  (def m1 (make-matrix 10 10 a))
)
