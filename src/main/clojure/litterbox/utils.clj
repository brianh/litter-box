(ns litterbox.utils)

(comment
  (load-file "/home/brian/code/clj/s4/src/main/clojure/litterbox/utils.clj")
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

(defn sleep [ms]
  "Calling thread sleeps for ms milliseconds.  Does not
   handle any interruptions for the caller."
  (Thread/sleep ms))

(defn sleep-rnd [ms]
  "Calling thread sleeps for [0 to ms).  Does not
   handle any interruptions for the caller."
  (sleep (rand ms)))

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

(defn ring-range [offset rng]
  "Returns a fn that returns a number between 0 and rng (exclusive) always
   with the specified offset."
  (fn [n]
    (mod (+ n offset) rng)))

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

(defn divisible-by [num div]
  (zero? (rem num div)))

(defn divisible-by-any [n s]
  (and (seq s) (reduce #(or %1 %2) (map (partial divisible-by n) s))))

(defn prime? [n]
  (cond (= n 2)
        true
        (divisible-by n 2)
        false
        :else
        (not (divisible-by-any n (range 3 (inc (int (Math/sqrt n))) 2)))))

(defn primes-between [n m]
  (filter prime? (range n m)))

(defn primes-to [n]
  (primes-between 2 n))

(defn prime-factors [n]
  (loop [cur-n n
         ps (primes-to (inc (/ n 2)))
         cur-prime (first ps)
         fs [1]]
    (if (seq ps)
      (if (divisible-by cur-n cur-prime)
        (recur (/ cur-n cur-prime) (primes-between cur-prime (inc cur-n))  cur-prime (conj fs cur-prime))
        (recur cur-n (rest ps) (first ps) fs))
      (conj fs n))))

(defn count-factors [p s]
  (count (filter (partial = p) s)))

(comment
(for [x (range 1 25)
      :let [xfactors (prime-factors x)]]
  (println 
   (apply str x "\t" (interpose "\t" (map #(count-factors % xfactors) (range 1 (inc x)))))))
)

(defn map-difference [m1 m2]
  (let [ks (keys m1)]
    (reduce dissoc m1 (filter identity (map (fn [k] (if (= (k m1) (k m2))
                                                      k
                                                      nil))
                                            ks)))))

;(reduce #(apply assoc %1 %2) {} (filter (fn [e] (> (val e) 60)) mb))
