(defn distance [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1), dy (- y2 y1)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn brute-force [points]
  (let [n (count points)]
    (when (< 1 n)
      (apply min-key first
             (for [i (range 0 (dec n)), :let [p1 (nth points i)],
                   j (range (inc i) n), :let [p2 (nth points j)]]
               [(distance p1 p2) p1 p2])))))

(defn combine [yS [dmin pmin1 pmin2]]
  (apply min-key first
         (conj (for [[p1 p2] (partition 2 1 yS)
                     :let [[_ py1] p1 [_ py2] p2]
                     :while (< (- py1 py2) dmin)]
                 [(distance p1 p2) p1 p2])
               [dmin pmin1 pmin2])))

(defn closest-pair
  ([points]
     (closest-pair
      (sort-by first points)
      (sort-by second points)))
  ([xP yP]
     (if (< (count xP) 4)
       (brute-force xP)
       (let [[xL xR] (partition-all (Math/ceil (/ (count xP) 2)) xP)
             [xm _] (last xL)
             {yL true yR false} (group-by (fn [[px _]] (<= px xm)) yP)
             dL&pairL (closest-pair xL yL)
             dR&pairR (closest-pair xR yR)
             [dmin pmin1 pmin2] (min-key first dL&pairL dR&pairR)
             {yS true} (group-by (fn [[px _]] (< (Math/abs (- xm px)) dmin)) yP)]
         (combine yS [dmin pmin1 pmin2])))))
