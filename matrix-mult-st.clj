(comment

  (load-file "C:\\home\\lisp\\clj\\src\\git-repos\\litterbox\\matrix-mult-st.clj")
  (def m1 (sqr-matrix 50 (repeatedly (partial rand-int 2))))
  (def m2 (sqr-matrix 50 (repeatedly (partial rand-int 2))))
  (def m1 (sqr-matrix 256 (rands)))
  (def m2 (sqr-matrix 256 (rands)))
  (def m3 (m-mult m1 m2))
  (time (do (doall (:data m3)) nil))
  (time (reduce unchecked-add (map unchecked-multiply (range 100) (repeat 1))))
  (time (reduce + (map * (range 100) (repeat 1))))
)

(defn sqr-matrix [n coll]
  (make-matrix n n coll))

(defn make-matrix
  ( [r c]
      (make-matrix r c (repeat 0)))
  ( [r c coll]
      {:rows r :cols c :data (partition c (take (* r c) coll))}))
  
(defn rands []
  (repeatedly rand))

(defn flatten
  (
   [coll]
     (let [f (first coll)
	   n (next coll)]
	 (if (coll? f)
	   (concat (flatten f) (flatten n))
	   (if f
	     (cons f (flatten n))
	     [])))))

;(into [] (map #(let [r (.get % 0)] @r) (:data m1)))
;(vec (map #(let [r (.get % 0)] @r) (:data m1)))

(defn get-col [m c]
   (map #(let [r (nth % c)] r) (:data m)))

(defn get-cols [m]
  (for [c (range (:cols m))]
    (get-col m c)))

(defn matrix-transpose [{rs :rows cs :cols :as m}]
  (make-matrix cs rs (get-cols m)))
		   
;(defn get-row [m r]
 ; (nth (:data m) r))

;(defn matrix-mult [m1 m2]
;  {:rows (:rows m1) :cols (:cols m2)
;   :data (vec (partition (:cols m2) (for [r (:data m1) c (get-cols m2)]
;	   (reduce + (map #(* %1 %2) r c)))))})

;(defn matrix-mult [m1 m2]
;  {:rows (:rows m1) :cols (:cols m2)
;   :data (vec (partition (:cols m2) (for [r (map @ (:data m1)) c (map @ (get-cols m2))]
;	   (reduce + (map #(* %1 %2) r c)))))})

(defn mul-n-add [row col] (reduce unchecked-add (map unchecked-multiply row col)))

(defn m-mult [m1 m2]
  (make-matrix (:rows m1) (:cols m2)
	       (for [r (:data m1)
		     c (:data m2)]
;		     c (get-cols m2)]  order of magnitude hit in performance....
		 (reduce + (pmap * r c)))))

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

