(ns litterbox.golf)
  ;  (:require [clojure.set-out :as set]))


(def golfers [:brian :bill :mike :andy :chuck :chris :jeff :rich])

(def pairing-pool (atom (shuffle (for [g golfers
				       g2 golfers
				       :when (not= g g2)]
				   #{g g2}))))

(def used-pairings (atom #{}))

(def flight-prs (atom #{}))

(defn select-pair []
  (when-let [nxt (first (drop-while #(some @flight-prs %)
				    (filter (complement #(contains? @used-pairings %)) @pairing-pool)))]
					;    (drop-while #(contains? @used-pairings %) @pairing-pool)))]
    (swap! flight-prs into nxt)
    nxt))

(defn build-round [num-carts]
  (let [flight (into [] (repeatedly num-carts select-pair))]
   ;(println "Flight=" flight)
    (swap! used-pairings into flight)
    (swap! flight-prs (constantly #{}))
    flight))

(defn build-tournament [num-rounds num-carts forbidden-pairings]
  (swap! pairing-pool shuffle)
  (swap! used-pairings (constantly (into #{} forbidden-pairings)))
  (swap! flight-prs (constantly #{}))
  (into [] (repeatedly num-rounds (partial build-round num-carts)))) 
