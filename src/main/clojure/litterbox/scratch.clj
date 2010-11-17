(def a {:x 3 :y 5 :emp {:name "bob"
			:age 19
			:address {:street "420 Main"
				  :city "Somewhere"
				  :state :VA}}})

(reduce #(assoc-in %1 (first %2) (second %2)) a
	(map vector
	     [[:x] [:emp :age] [:emp :address :state]]
	     [9 20 :NY]))

(defn
 #^{:arglists '([map key val] [map key val & kvs])
    :doc "assoc[iate]. When applied to a map, returns a new map of the
    same (hashed/sorted) type, that contains the mapping of key(s) to
    val(s). When applied to a vector, returns a new vector that
    contains val at index. Note - index must be <= (count vector)."}
 my-assoc
 ([map key val] (if (coll? key)
		  (let [curmap (get map (first key))]
			(assoc-in curmap (rest key) val))
		  (assoc map key val)))
 ([map key val & kvs]
    (let [ret (my-assoc map key val)]
      (if kvs
        (recur ret (first kvs) (second kvs) (nnext kvs))
        ret))))

(reduce get a ks)

(map (partial reduce get a) (buildup ks))

(defn buildup [coll]
  (for [n (iterate inc 1) :let [c (count coll)] :while (<= n c)] (take n coll)))

(for [n t] (take-while (complement #{n}) t))


(map (partial get a) (for [k ks] (

				  
(map vec (reduce (fn [m k]
		   (let [v (get m k)]
		     (if (coll? v)
		     v
		     nil)) a ks)))

(defn accessor [a]
  (cond
    (number? a) (fn [c] (nth c a))
    (symbol? a) (fn [c] (get c a))
    
    :else nil)
   
(defn replace-all [orig deltas]
  (reduce (fn [c [a b]] (assoc-in c a b)) orig deltas))

	  (map vector
	       [[:x] [:emp :age] [:emp :address :state]]
	       [9 20 :NY]))

(into [] (map ref (take 20 (repeatedly (partial rand-int 1000)))))
(into [] (repeat 5 (into [] (map ref (take 5 (repeatedly (partial rand-int 1000)))))))
