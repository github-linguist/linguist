(defn flip-at [n coll]
  (let [[x y] (split-at n coll)]
    (concat (reverse x) y )))

(def sorted '(1 2 3 4 5 6 7 8 9))  ; i.e. (range 1 10)
(def unsorted? #(not= % sorted))

(loop [unsorted (first (filter unsorted? (iterate shuffle sorted))), steps 0]
  (if (= unsorted sorted)
    (printf "Done! That took you %d steps%n" steps)
    (do
      (println unsorted)
      (printf "Reverse how many? ")
      (flush)
      (let [flipcount (read)]
        (recur (flip-at flipcount unsorted), (inc steps))))))
