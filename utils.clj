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

(let [f (future (dosync (let [v1 @r1 v2 @r2] (display "Future executing..." v1 v2) (ensure r1)
			(ignore [InterruptedException] (sleep 15000)) (alter r1 * v1) (alter r2 inc) (display "Future post alter" @r1 @r2))))]
  (ignore [InterruptedException]
	  (sleep 1000))
  (dosync (let [v1 @r1 v2 @r2] (ensure r2) (display "Main thread txn" v1 v2)
   (alter r1 + v1) (alter r2 inc) (display "Future post alter" @r1 @r2)))
  (display "Post-txn:" @f "...." @r1 @r2))

(let [f (future (do
		  (dosync (let [v1 @r1 v2 @r2]
			    (display "Future executing..." v1 v2)
			    (ensure r1)
			    (ensure r2)
			    (ignore [InterruptedException] (sleep 15000))
			    (alter r1 * v1)
			    (alter r2 inc)
			    (display "Future post alter" @r1 @r2)
			    (alter r3 (fn[_] (* @r1 @r2)))))
		  (display "Future r3-->" @r3)))]
  (ignore [InterruptedException]
	  (sleep 1000))
  (dosync (let [v1 @r1 v2 @r2] (ensure r2)(ensure r1) (display "Main thread txn" v1 v2)
   (alter r1 + v1)
   (alter r2 inc)
   (display "Main post alter" @r1 @r2)
   (alter r3 (fn[_] (+ @r1 @r2)))))
  (display "Main r3 =>" @r3)
  (display "Post-txn:" @f "...." @r1 @r2))

(let [f (future (dosync (prn "executing...." @r1)
			(ignore [InterruptedException] (sleep 15000)) (alter r2 inc)))]
  (ignore [InterruptedException]
	  (sleep 1000))
  (prn "In-txn Ref value:" @r1)
  (dosync
   (ensure r2)
   (alter r1 inc))
  (prn "Post-txn:" @f))

(let [f (future (do
		  (dosync (let [v1 @r1 v2 @r2]
			    (display "Future executing..." v1 v2)
			    (ensure r1)
			    (ensure r2)
			    (ignore [InterruptedException] (sleep 15000))
			    (let [nr1 (alter r1 * v1)
			    nr2 (alter r2 inc)]
			    (alter r3 (fn[v] (+ v (* nr1 nr2)))))))
		  (display "Future post-txn -->" @r3)))]
  (ignore [InterruptedException]
	  (sleep 1000))
  (dosync (let [v1 @r1 v2 @r2] (ensure r2)(ensure r1) (display "Main thread txn" v1 v2)
   (let [nr1 (alter r1 + v1)
   nr2 (alter r2 inc)]
   (alter r3 (fn[v] (+ v nr1 nr2))))))
  (display "Main Post-txn:...." @r3))
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
