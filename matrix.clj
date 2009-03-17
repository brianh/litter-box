(ns matrix '(import '(java.util Arrays)))

(defstruct matrix :nrows :ncols :data :Type)

(defn make-matrix [numrows numcols data]
      (struct matrix numrows numcols data :true :Matrix))

(defn get-col [m c]
  (let [{data :data step :ncols} m
	cnt (count data)]
    (apply vector (map (partial nth data) (range c cnt step)))))

(defn get-cols [m]
  (apply vector (flatten (for [c (range (:ncols m))]
			   (get-col m c)))))

(defn get-row [m r]
  (let [{:keys [ncols data]} m
	start (* r ncols)]
    (subvec data start (+ start ncols))))

; uses modulo arithmatic... faster than the transpose1 fn
(defn transpose [{rs :nrows cs :ncols data :data :as m}]
  (let [cnt (count data)
	iter-rng (- cnt 1)]
    (conj (apply vector (map (partial nth data) 
			     (take iter-rng (iterate (fn [x] (mod (+ x cs) iter-rng)) 0))))
	  (nth data iter-rng))))

(defn transpose1 [{rs :nrows cs :ncols data :data :as m}]
  (make-matrix cs rs (get-cols m)))

(defn m-mult [m1 m2]
  (make-matrix (:rows m1) (:cols m2)
	       (for [r (:data m1)
		     c (:data m2)]
		 (reduce + (map * r c)))))

(defn aprn [a]
  (doseq [r a]
    (prn (apply vector r))))

(defn mprn [m]
  (aprn (map (partial get-row m) (range (:nrows m)))))


(defn am-mult-hinted [m1 m2]
  (make-matrix (:rows m1) (:cols m2)
	       (for [r (:data m1)
		     c (:data m2)
		     :let [cnt (count r)]]
		 (loop [i (int 0)
			result (double 0)]
		   (if (>= i cnt)
		     result
		     (recur (inc i) (double (+ result (* (aget r i) (aget c i))))))))  into-array))
		     ;(recur (inc i) (unchecked-add result (unchecked-multiply (aget r i) (aget c i)))))))  into-array))

(defn r-c-mult [row col]
  (loop [i (int (- (count row) 1))
	 result (double 0)]
    (if (< i 0)
      result
      (recur (dec i) (double (+ result (* (aget row i) (aget col i))))))))

(defn am-mult-hinted2 [m1 m2]
  (make-matrix (:rows m1) (:cols m2)
	       (for [r (:data m1)
		     c (:data m2)
		     :let [cnt (count r)]]
		 (r-c-mult r c)) into-array))


(comment

  (load-file "C:\\home\\lisp\\clj\\src\\git-repos\\litterbox\\matrix.clj")
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

;;;; old crap
(comment
(defn amult2 [rs cs]
  (amap rs i ret
    (* (aget ret i) (aget cs i))))

(defn asum [ xs]
  (areduce xs i ret 0
    (+ ret (aget xs i))))

(loop [i (int 0)
       result (int 0)]
  (if (>= i 100)
    result
    (recur (inc i) (unchecked-add result (unchecked-multiply i 1 )))))

(defn m-mult1 [m1 m2]
  (make-matrix (:rows m1) (:cols m2)
	       (for [r (:data m1)
		     c (:data m2)]
;		     c (get-cols m2)]  order of magnitude hit in performance....
		 (reduce unchecked-add (map unchecked-multiply r c)))))
)