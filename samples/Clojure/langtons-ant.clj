(let [bounds (set (range 100))
      xs [1 0 -1 0] ys [0 -1 0 1]]
  (loop [dir 0 x 50 y 50
         grid {[x y] false}]
    (if (and (bounds x) (bounds y))
      (let [cur (not (grid [x y]))
            dir (mod (+ dir (if cur -1 1)) 4)]
        (recur dir (+ x (xs dir)) (+ y (ys dir))
               (merge grid {[x y] cur})))
      (doseq [col (range 100)]
        (println
          (apply str
                 (map #(if (grid [% col]) \# \.)
                      (range 100))))))))
