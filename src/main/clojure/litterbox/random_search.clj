(ns litterbox.random-search)

(comment 
  (load-file "/home/brian/code/clj/litter-box/src/main/clojure/litterbox/random_search.clj")
  (in-ns 'litterbox.random-search)
  )

(defn rand-coord [min max rand-fn]
  (let [span (- max min)
	
  (fn []
    (- (rand-fn (- max min)
		  
(defrecord Point2I [^int x ^int y])

(defrecord Solution [value cost])

(defprotocol Costable
  (calc-cost [this cost-fn] "Calculates the cost."))

(extend-protocol Costable
  Solution
  (calc-cost [this cost-fn]
	     (:cost (cost-fn (:value this)))))

(defn make-solution []
  (let [x (- (rand-int 10) 5)
	y (- (rand-int 10) 5)