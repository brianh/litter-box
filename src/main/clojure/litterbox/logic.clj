(ns litterbox.logic
	(:refer-clojure :exclude [==])
	(:require [clojure.core.logic.fd :as fd])
  (:use (clojure.core logic)))

;(use 'clojure.core.logic)

(run* [q]
      (membero q [1 2 3])
      (membero q [2 3 4]))


;(require '[clojure.core.logic.fd :as fd])

(run* [q]


      (fd/in q (fd/interval 1 5)))


(run* [r]
			(fresh [x y]
             (fd/in x y (fd/interval 1 5))
             (fd/+ x y 10)
             (== r [x y])))

(run* [q]
			(!= q [1 2])
			(membero q [1 2]))

(run* [q]
			(fresh [s t]
						 (== q s)
			(firsto t q)))