(ns litterbox.geom)

(comment 
  (load-file "/home/brian/code/clj/litterbox/src/main/clojure/litterbox/geom.clj")
  (in-ns 'litterbox.geom))

(defrecord Point2D [^double x ^double y])

(defprotocol VectorSpace
  (scale [this s] "Scales the vector by s.")
  (add [this v] "Add v to this vector.")
  (sub [this v] "Subtract v from this vector."))

(extend-protocol VectorSpace
  Point2D
  (scale [this s]
	 (Point2D. (* (:x this) s) (* (:y this) s)))
  (add [this v]
       (Point2D. (+ (:x this) (:x v)) (+ (:y this) (:y v))))
  (sub [this v]
       (Point2D. (- (:x this) (:x v)) (- (:y this) (:y v)))))


(comment
  (def p1 (Point3D. 1.0 0.0 0.0))
  (def p2 (Point3D. 0.0 1.0 0.0))
  (cross v1 v2)
  )