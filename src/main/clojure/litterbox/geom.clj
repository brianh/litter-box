(ns litterbox.geom)

(comment 
  (load-file "/home/brian/code/clj/litterbox/src/main/clojure/litterbox/geom.clj")
  (in-ns 'litterbox.geom))

(defrecord Point2D [^double x ^double y])
(defrecord Point3D [^double x ^double y ^double z])

(defprotocol VectorSpace
  (scale [this s] "Scales the vector by s.")
  (add [this v] "Add v to this vector.")
  (sub [this v] "Subtract v from this vector."))

(extend-protocol VectorSpace
  ;;--
  Point2D
  ;;--
  (scale [this s]
	 (Point2D. (* (:x this) s) (* (:y this) s)))
  (add [this v]
       (Point2D. (+ (:x this) (:x v)) (+ (:y this) (:y v))))
  (sub [this v]
       (Point2D. (- (:x this) (:x v)) (- (:y this) (:y v))))
  ;;--
  Point3D
  ;;--
  (scale [this s]
	 (Point3D. (* (:x this) s) (* (:y this) s) (* (:z this) s )))
  (add [this v]
       (Point3D. (+ (:x this) (:x v)) (+ (:y this) (:y v)) (+ (:z this) (:z v))))
  (sub [this v]
       (Point3D. (- (:x this) (:x v)) (- (:y this) (:y v)) (- (:z this) (:z v)))))

(defprotocol EuclideanSpace
  (dot [this v] "Dot product of the two vectors.")
  (mag-sqr [this] "Magnitude squared of this vector.")
  (mag [this] "Magnitude of this vector.")
  (proj [this v] "Projection of this vector onto v.")
  (cross [this v] "Cross product of this vector with v.")
  (make-unit [this] "Creates a new unit vector from this vector."))

(extend-protocol EuclideanSpace
  ;;--
  Point2D
  ;;--
  (dot [this v]
       (* (:x this) (:x v)) (* (:y this) (:y v)))
  (mag-sqr [this]
	   (reduce + (map #(* % %) (vals this))))
  (mag [this]
       (Math/sqrt (mag-sqr this)))
  (cross [this v]
	 (Point3D. 0.0 0.0 (- (* (:x this) (:y v))
			       (* (:y this) (:x v)))))
  (make-unit [this]
	     (scale this (/ 1 (mag this))))
  ;;--
  Point3D
  ;;--
  (dot [this v]
       (* (:x this) (:x v)) (* (:y this) (:y v)) (* (:z this) (:z v)))
  (mag-sqr [this]
	   (reduce + (map #(* % %) (vals this))))
  (mag [this]
       (Math/sqrt (mag-sqr this)))
  (cross [this v]
	 (Point3D. (- (* (:y this) (:z v))
		       (* (:z this) (:y v)))
		    (- (* (:x this) (:z v))
		       (* (:z this) (:x v)))
		    (- (* (:x this) (:y v))
		       (* (:y this) (:x v)))))
  (make-unit [this]
	     (scale this (/ 1 (mag this)))))

(comment
  (def v1 (Point3D. 1.0 0.0 0.0))
  (def v2 (Point3D. 0.0 1.0 0.0))
  (cross v1 v2)
  )