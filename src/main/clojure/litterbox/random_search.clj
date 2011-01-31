(ns litterbox.random-search)

(comment 
  (load-file "/home/brian/code/clj/litter-box/src/main/clojure/litterbox/random_search.clj")
  (in-ns 'litterbox.random-search)
  )

(defprotocol Costable
  (calc-cost [this] "Calculates the cost."))

(defrecord Point2d [x y]
  Costable
  (calc-cost [this]
	     (+ (Math/pow (:x this) 2.0)
		(Math/pow (:y this) 2.0))))

(defn bounded-rand-generator [rand-fn min max]
  (let [xspan (- max min)]
    (fn []
      (- (rand-fn xspan) (/ xspan 2)))))

(def r-gen (bounded-rand-generator rand -5 5))

(def solution-space (map #(Point2d. %1 %2) (repeatedly r-gen) (repeatedly r-gen)))

(defn find-solution [n best]
  (if (= n 0)
    best
    ()))


(comment
  (take 2 (map #(Point2d. %1 %2) (repeatedly r-gen) (repeatedly r-gen)))
  )