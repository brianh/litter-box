(def a {:x 3 :y 5 :emp {:name "bob"
			:age 19
			:address {:street "420 Main"
				  :city "Somewhere"
				  :state :VA}}})

(reduce #(assoc-in %1 (first %2) (second %2)) a
	(map vector
	     [[:x] [:emp :age] [:emp :address :state]]
	     [9 20 :NY]))

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

(defn my-reduce
  "f should be a function of 2 arguments. If val is not supplied,
  returns the result of applying f to the first 2 items in coll, then
  applying f to that result and the 3rd item, etc. If coll contains no
  items, f must accept no ar(guments as well, and reduce returns the
  result of calling f with no arguments.  If coll has only 1 item, it
  is returned and f is not called.  If val is supplied, returns the
  result of applying f to val and the first item in coll, then
  applying f to that result and the 2nd item, etc. If coll contains no
  items, returns val and f is not called."
  ([f coll]
   (let [s (seq coll)]
     (if s
       (if (instance? clojure.lang.IReduce s)
         (. #^clojure.lang.IReduce s (reduce f))
         (reduce f (first s) (next s)))
       (f))))
  ([f val coll]
     (let [s (seq coll)]
       (if (instance? clojure.lang.IReduce s)
         (. #^clojure.lang.IReduce s (reduce f val))
         ((fn [f val s]
            (if s
              (recur f (f val (first s)) (next s))
              val))
          f val s)))))