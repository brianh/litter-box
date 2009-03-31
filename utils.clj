(ns litterbox.utils)

(comment
  (load-file "C:\\home\\lisp\\clj\\src\\git-repos\\litterbox\\utils.clj")
)
;, :nskeys #(keyword (ns-name *ns*) str %))})]
;, :nskeys #(keyword (str (ns-name *ns*)) (str %))})]
;(keyword (str (ns-name *ns*)) "aoeu")

(comment
(reduce
 (fn [bes entry]
   (reduce #(assoc %1 %2 ((val entry) %2))
	   (dissoc bes (key entry))
	   ((key entry) bes)))
 {:nskeys [nsx nsy] :keys [x y]}
 {:keys #(keyword (str %)), :strs str, :syms #(list `quote %),
  :nskeys #(keyword (str (ns-name *ns*)) (str %))})
)

(def display-lock (ref 0))

(defn display [& args]
  "Ensures write order"
  (let [output (apply str (interpose " " args))]
    (dosync
     (ensure display-lock)
     (println output))))

(defn rands []
  (repeatedly rand))

(defn flatten [coll]
  "Depth first tree flattening into a collection"
  (let [f (first coll)
	n (next coll)]
    (if (first f)
      (concat (flatten f) (flatten n))
      (if f
	(cons f (flatten n))
	[]))))

(defn sleep [millis]
  "Calling thread sleeps for millis milliseconds.  Does not
   handle any interruptions for the caller."
  (Thread/sleep millis))

(defn sleep-rnd [millis]
  "Calling thread sleeps for [0 to millis).  Does not
   handle any interruptions for the caller."
  (sleep (rand millis)))

(defmacro ignore [es form]
  `(try ~form
	~@(map #(list 'catch % 'e nil) es)))

(defn when-all
  ;( [access pred a b] should extend to multiple colls
   ;   (
  ( [access pred mcoll]
      (let [vs (map (fn [m] (get-in m access)) mcoll)]
	(if (apply pred vs)
	  vs
	  nil))))

(defmacro handle
  ( [es form]
  `(try ~form
	~@(map (fn [[a e b]] (list 'catch a e b)) (partition 3 es))))
  ( [es form final]
  `(try ~form
	~@(map (fn [[a e b]] (list 'catch a e b)) (partition 3 es))
	(finally ~final))))

(defn spawn [f]
  (.start (Thread. f)))
   
(defn replace-all [orig deltas]
  (reduce (fn [c [a b]] (assoc-in c a b)) orig deltas))

(defn buildup [coll]
  (let [cnt (count coll)]
    (take cnt (map (fn [n] (take n coll)) (iterate inc 1)))))

(defn ring-range [step rng]
  (fn [n]
    (mod (+ n step) rng)))

(defn repeat-each [n coll]
  (mapcat (fn [x] (repeat n x)) coll))

(defn map-difference2 [m1 m2]
  (reduce (fn [m entry] (let [k (key entry)
			      v1 (val entry)
			      v2 (k m2)]
			  (if (= v1 v2)
			    (dissoc m k)
			    m)))
	  m1 m1))

(defn map-difference [m1 m2]
  (let [ks (keys m1)]
    (reduce dissoc m1 (filter identity (map (fn [k] (if (= (k m1) (k m2))
						      k
						      nil))
					    ks)))))

;(reduce #(apply assoc %1 %2) {} (filter (fn [e] (> (val e) 60)) mb))
