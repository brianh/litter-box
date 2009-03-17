(ns amatrix)

(defstruct matrix :nrows :ncols :data)

(defn print-matrix [m]
  (print (apply str (map (fn [a] (apply println-str (interpose " " a))) (:data m)))))

(defn make-2d-array [numrows numcols afn coll]
  (into-array (map afn (partition numrows (take (* numrows numcols) coll)))))

(defn make-matrix
  ( [numrows numcols afn def-val]
      (make-matrix (make-2d-array numrows numcols afn (repeat def-val))))
  ( [data]
      (struct matrix (count data) (count (aget data 0)) data)))

(defn partition-matrix [m nrows ncols]
  (

;;----------------
;; double
(defn asum-double [#^doubles xs]
  (areduce xs i ret (double 0)
    (+ ret (double (aget xs i)))))

(defn amult-sum-double [#^doubles v1 #^doubles v2]
  (asum-double (amap v1 i ret (* (double (aget ret i)) (double (aget v2 i))))))

(defn mult-double-atom [m1 m2]
  (let [nrows (:nrows m1)
	ncols (:nrows m2)
	d1 (:data m1)
	d2 (:data m2)
	m (atom (make-matrix nrows ncols double-array 0))]
    (doseq [r (range nrows)
	    c (range ncols)]
      (send m (fn[m #^doubles row #^double col]
		(let [v (amult-sum-double row col)]
		  ;(prn "starting...." r c "=" v)
		  (aset (:data m) r c v)
		  ;(prn "m value" (aget (:data m) r c))
		  m))
	    (doubles (aget d1 r))
	    (doubles (aget d2 c))))
    (await m)
    @m))

(defn mult-double-agent [m1 m2]
  (let [nrows (:nrows m1)
	ncols (:nrows m2)
	d1 (:data m1)
	d2 (:data m2)
	m (agent (make-matrix nrows ncols double-array 0))]
    (doseq [r (range nrows)
	    c (range ncols)]
      (send m (fn[m #^doubles row #^double col]
		(let [v (amult-sum-double row col)]
		  ;(prn "starting...." r c "=" v)
		  (aset (:data m) r c v)
		  ;(prn "m value" (aget (:data m) r c))
		  m))
	    (doubles (aget d1 r))
	    (doubles (aget d2 c))))
    (await m)
    @m))

(defn mult-double-good [m1 m2]
  (let [nrows (:nrows m1)
	ncols (:nrows m2)
	d1 (:data m1)
	d2 (:data m2)]
    (make-matrix
     (make-2d-array nrows ncols double-array
		    (for [#^doubles r d1
			  #^doubles c d2]
		      (amult-sum-double r c))))))

(defn transpose-double [{:keys [nrows ncols data]}]
  (make-matrix (make-2d-array ncols nrows double-array (for [c-idx (range ncols)
							  r-idx (range nrows)]
						      (aget data r-idx c-idx)))))

;;------------------
;; int
(defn asum-int [#^ints xs]
  (areduce xs i ret (int 0)
    (unchecked-add ret (int (aget xs i)))))

(defn amult-sum-int [#^ints v1 #^ints v2]
  (asum-int (amap v1 i ret (unchecked-multiply (int (aget ret i)) (int (aget v2 i))))))

(defn mult-int [m1 m2]
  (let [nrows (:nrows m1)
	ncols (:nrows m2)
	d1 (:data m1)
	d2 (:data m2)]
    (make-matrix
     (make-2d-array nrows ncols int-array
		    (for [#^ints r d1
			  #^ints c d2]
		      (amult-sum-int r c))))))

(defn transpose-int [{:keys [nrows ncols data]}]
  (make-matrix (make-2d-array ncols nrows int-array (for [c-idx (range ncols)
							  r-idx (range nrows)]
						      (aget data r-idx c-idx)))))

(comment
  (load-file "C:\\home\\lisp\\clj\\src\\git-repos\\litterbox\\amatrix.clj")
  (def b (into-array (map double-array (partition 10 (take 100 (repeatedly (partial rand-int 2)))))))
  (def m1 (make-matrix 10 10 int-array 0))
  
  (def msize 3)
  (def msize 50)
  (def msize 100)
  (def msize 150)
  (def msize 256)
  (def msize 512)
  (def msize 1024)
  (def rng 2)
  (def rng 10)
  (def bfn int-array)
  (def bfn double-array)
  
  (def m1 (make-matrix (make-2d-array msize msize bfn (repeatedly (partial rand-int rng)))))
  (def m2 (make-matrix (make-2d-array msize msize bfn (repeatedly (partial rand-int rng)))))
  
  (def m3 (make-matrix (make-2d-array 10 10 double-array (repeatedly rand))))
)

;;----------------
;; old
(comment
  
(defn mult-best [m1 m2]
  (let [nrows (:nrows m1)
	ncols (:nrows m2)
	d1 (:data m1)
	d2 (:data m2)
	result (into-array (map int-array (repeat nrows ncols)))]
    (make-matrix
     (loop [ridx (int 0)]
       (if (>= ridx nrows)
	 result
	 (do
	   (loop [cidx (int 0)]
	     (if (>= cidx ncols)
	       nil
	       (do
		 (aset result ridx cidx (int (amult-sum (ints (aget d1 ridx)) (ints (aget d2 cidx)))))
		 (recur (inc cidx)))))
	   (recur (inc ridx))))))))

(defn mult-2 [m1 m2]
  (let [nrows (:nrows m1)
	ncols (:nrows m2)
	d1 (:data m1)
	d2 (:data m2)
	result (into-array (map int-array (repeat nrows ncols)))]
    (make-matrix
     (loop [ridx (int 0)]
       (if (>= ridx nrows)
	 result
	 (do
	   (loop [cidx (int 0)]
	     (if (>= cidx ncols)
	       nil
	       (do
		 (aset result ridx cidx (rc-mult (ints (aget d1 ridx)) (ints (aget d2 cidx))))
		 (recur (inc cidx)))))
	   (recur (inc ridx))))))))
  
(defn print-array [a]
  (apply println (map str a)))
	 
)