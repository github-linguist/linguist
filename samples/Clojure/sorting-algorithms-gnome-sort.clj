(defn gnomesort
  ([c] (gnomesort c <))
  ([c pred]
     (loop [x [] [y1 & ys :as y] (seq c)]
       (cond (empty? y) x
             (empty? x) (recur (list y1) ys)
             true (let [zx (last x)]
                    (if (pred y1 zx)
                      (recur (butlast x) (concat (list y1 zx) ys))
                      (recur (concat x (list y1)) ys)))))))

(println (gnomesort [3 1 4 1 5 9 2 6 5]))
