(ns litterbox.simulation)

(comment
  (load-file "/home/brian/code/clj/litter-box/src/main/clojure/litterbox/simulation.clj")
  (in-ns 'litterbox.simulation)
  )

(defprotocol Model
  (init [this])
  (exec [this])
  (cleanup [this]))

(def *terrain* (ref #{}))
(def *actors* (ref #{}))
(def *time* (atom 0))

(defn sense [sensor]
  (println (:name sensor) " being sensitive..."))

(defn think [thinker]
  (println (:name thinker) " pensing hard..."))

(defn act [actor]
  (println (:name actor) " to be or not to be..."))

(defn add-actor [name loc]
  (dosync
   (alter *actors* into {:name name :location loc})))

(defn run [end-time]
  (dotimes [n end-time]
    (map sense *actors*)
    (map think *actors*)
    (map act *actors*)
    (swap! *time* inc)))


