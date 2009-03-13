(comment

  (load-file "C:\\home\\lisp\\clj\\src\\git-repos\\litterbox\\matrix-mult-st.clj")
  (def m1 (sqr-matrix 50 (repeatedly (partial rand-int 2))))
  (def m2 (sqr-matrix 50 (repeatedly (partial rand-int 2))))
  (def m1 (sqr-matrix 256 (rands) into-array))
  (def m2 (sqr-matrix 256 (rands) into-array))
  (def m3 (am-mult-hinted m1 m2))
  (time (do (doall (:data m3)) nil))
  (time (reduce unchecked-add (map unchecked-multiply (range 100) (repeat 1))))
  (time (reduce + (map * (range 100) (repeat 1))))
  
  (def m1 (sqr-matrix 5 (repeatedly (partial rand-int 10)) into-array))
)
(import '(java.util Arrays))

(defn
  #^doubles
  make-matrix
  ( [r c]
      (make-matrix r c (repeat 0)))
  ( [r c coll]
      (into-array (take (* r c) coll))))

(defn sqr-matrix [n coll f]
  (make-matrix n n coll f))
  
(defn rands []
  (repeatedly rand))

(defn flatten [coll]
  "Depth first tree flattening into a collection"
  (let [f (first coll)
	n (next coll)]
    (if (coll? f)
      (concat (flatten f) (flatten n))
      (if f
	(cons f (flatten n))
	[]))))

(defn get-col [m c]
   (map #(let [r (nth % c)] r) (:data m)))

(defn get-cols [m]
  (for [c (range (:cols m))]
    (get-col m c)))

(defn matrix-transpose [{rs :rows cs :cols :as m}]
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
  (aprn (:data m)))

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

;(defn matrix-deref [m]
;  (for [r (:data m)
;	rd r]
;    @rd))

;(defn matrix-deref [m]
;  (for [r (:data m)]
;    (map deref r)))

