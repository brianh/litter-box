(ns litterbox.geom)

(comment 
  (load-file "/home/brian/code/clj/litterbox/src/main/clojure/litterbox/geom.clj")
  (in-ns 'litterbox.geom))

(defrecord Point2d [^double x ^double y])
(defrecord Point3d [^double x ^double y ^double z])

(defprotocol Vector
  (scale [this s])
  (dot [this v])
  (add [this v])
  (mag [this])
  (cross [this v])
  (make-unit [this])
  (rank [this]))

(defrecord Vector3d [x y z])

(defrecord Vector2d [x y]
  Vector
  (scale [this s]
	 (Vector2d. (* (:x this) s) (* (:y this) s)))
  (dot [this v]
       (* (:x this) (:x v)) (* (:y this) (:y v)))
  (add [this v]
       (Vector2d. (+ (:x this) (:x v)) (+ (:y this) (:y v))))
  (mag [this]
       (Math/sqrt (reduce + (map #(* % %)
				 (vals this)))))
  (cross [this v]
	 (Vector3d. 0.0 0.0 (- (* (:x this)
				  (:y v))
			       (* (:y this)
				  (:x v)))))
  (make-unit [this]
	     (scale this (/ 1 (mag this))))
  (rank [this] 2))
