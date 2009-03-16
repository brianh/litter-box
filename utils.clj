(ns 'utils)

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


(defmacro handle [es form]
  `(try ~form
	~@(map #(list 'catch % 'e nil) (fn [[a b]] (list 'catch a 'e ~b)) (partition 2 2 es))))

(defn spawn [f]
  (.start (Thread. f)))
   
(defn replace-all [orig deltas]
  (reduce (fn [c [a b]] (assoc-in c a b)) orig deltas))

(defn buildup [coll]
  (for [n (iterate inc 1) :let [c (count coll)] :while (<= n c)] (take n coll)))