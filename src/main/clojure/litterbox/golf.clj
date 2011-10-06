(ns litterbox.golf)
  ;  (:require [clojure.set-out :as set]))


(def golfers [:brian :bill :mike :andy :chuck :chris :jeff :rich])

(def pairing-pool (atom (into #{} (shuffle (for [g golfers
						 g2 golfers
						 :when (not= g g2)]
					     #{g g2})))))

(def used-pairings (atom #{}))

(def flight-prs (atom #{}))

(defn select-pair []
  (if-let [nxt (first (drop-while #(some @flight-prs %)
				  (filter (complement #(contains? @used-pairings %)) @pairing-pool)))]
				  ;    (drop-while #(contains? @used-pairings %) @pairing-pool)))]
    (do
      (swap! flight-prs into nxt)
      nxt)
    :nil))

(defn build-round [num-carts]
  (let [flight (into [] (repeatedly num-carts select-pair))]
    (println "Flight=" flight)
    (swap! used-pairings into flight)
    (swap! flight-prs (constantly #{}))
    flight))

(defn build-tournament [num-rounds num-carts]
  (into [] (repeatedly num-rounds (partial build-round num-carts)))) 
