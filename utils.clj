(ns litterbox.utils)

(comment
  (load-file "C:\\home\\lisp\\clj\\src\\git-repos\\litterbox\\utils.clj")
)

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
