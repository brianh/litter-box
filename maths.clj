
(defn a-to-n [a n] (rem (reduce * (repeat n a)) n))
(defn naturals [n] (take n (iterate inc 1)))
(defn hw [n] (map #(map (partial a-to-n %) (naturals n)) (naturals n)))
(defn print-hw [n] (print (apply str (map (partial apply prn-str) (hw n)))))
