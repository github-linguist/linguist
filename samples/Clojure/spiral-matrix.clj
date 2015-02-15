(defn spiral [n]
  (let [fmt (str " ~{~<~%~," (* n 3) ":;~2d ~>~}~%")
        counts (cons n (mapcat #(repeat 2 %) (range (dec n) 0 -1)))
        ones-and-ns (mapcat #(repeat %1 %2) counts (cycle [1 n -1 (- n)]))]
    (->> (map vector (range 0 (* n n)) (reductions + ones-and-ns))
         (sort-by second)
         (map first)
         (clojure.pprint/cl-format true fmt))))
