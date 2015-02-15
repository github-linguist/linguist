(def fibs (lazy-cat [1 1] (map + fibs (rest fibs))))

(defn z [n]
  (if (zero? n)
    "0"
    (let [ps (->> fibs (take-while #(<= % n)) rest reverse)
          fz (fn [[s n] p]
                (if (>= n p)
                  [(conj s 1) (- n p)]
                  [(conj s 0) n]))]
      (->> ps (reduce fz [[] n]) first (apply str)))))

(doseq [n (range 0 21)] (println n (z n)))
