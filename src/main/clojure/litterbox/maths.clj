(defn a-to-n [a n] (rem (reduce * (repeat n a)) n))
(defn naturals [n] (take n (iterate inc 1)))
(defn hw [n] (map #(map (partial a-to-n %) (naturals n)) (naturals n)))
(defn print-hw [n] (print (apply str (map (partial apply prn-str) (hw n)))))
(defn print-row [n] (apply str (interleave n (repeat ","))))
(defn print-table [n] (print (apply str (interleave (map print-row (hw n)) (repeat "\n")))))