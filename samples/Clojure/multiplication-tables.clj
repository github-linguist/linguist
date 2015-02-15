(let [size 12
      trange (range 1 (inc size))
      fmt-width (+ (.length (str (* size size))) 1)
      fmt-str (partial format (str "%" fmt-width "s"))
      fmt-dec (partial format (str "% " fmt-width "d"))]

  (doseq [s (cons
             (apply str (fmt-str " ") (map #(fmt-dec %) trange))
             (for [i trange]
               (apply str (fmt-dec i) (map #(fmt-str (str %))
                                           (map #(if (>= % i) (* i %) " ")
                                                (for [j trange] j))))))]
    (println s)))
