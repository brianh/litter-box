(defn make-matrix
  ( [r c]
      (make-matrix r c (partial repeat 0)))
  ( [r c f]
      {:rows r :cols c :data (into [] (take r (repeatedly #(into [] (map agent (take c (repeatedly f)))))))}))

(defn make-matrix
  ( [r c]
      (make-matrix r c (partial repeat 0)))
  ( [r c f]
      {:rows r :cols c :data (into [] (take r (repeatedly #(into [] (take c (repeatedly f))))))}))

(partial rand-int 1000)

(into [] (map #(let [r (.get % 0)] @r) (:data m1)))
(vec (map #(let [r (.get % 0)] @r) (:data m1)))

(defn get-cols [m]
  (for [c (range (:cols m))]
    (get-col m c)))
      

			   
(defn get-col [m c]
   (map #(let [r (nth % c)] r) (:data m)))

(defn get-row [m r]
  (nth (:data m) r))

(defn matrix-mult [m1 m2]
  {:rows (:rows m1) :cols (:cols m2)
   :data (vec (partition (:cols m2) (for [r (map @ (:data m1)) c (map @ (get-cols m2))]
	   (reduce + (map #(* %1 %2) r c)))))})

(defn matrix-mult [m1 m2]
  {:rows (:rows m1) :cols (:cols m2)
   :data (vec (partition (:cols m2) (for [r (map @ (:data m1)) c (map @ (get-cols m2))]
	   (reduce + (map #(* %1 %2) r c)))))})

(defn matrix-deref [m]
  (for [r (:data m)
	rd r]
    @rd))

(defn matrix-deref [m]
  (for [r (:data m)]
    (map deref r)))

