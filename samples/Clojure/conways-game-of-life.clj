(defn moore-neighborhood [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not (= [dx dy] [0 0]))]
    [(+ x dx) (+ y dy)]))

(defn step [set-of-cells]
  (set (for [[cell count] (frequencies (mapcat moore-neighborhood set-of-cells))
             :when (or (= 3 count)
                       (and (= 2 count) (contains? set-of-cells cell)))]
         cell)))

(defn print-world
  ([set-of-cells] (print-world set-of-cells 10))
  ([set-of-cells world-size]
     (let [r (range 0 (+ 1 world-size))]
       (pprint (for [y r] (apply str (for [x r] (if (set-of-cells [x y]) \# \.))))))))

(defn run-life [world-size num-steps set-of-cells]
  (loop [s num-steps
         cells set-of-cells]
    (print-world cells world-size)
    (when (< 0 s)
      (recur (- s 1) (step cells)))))

(def *blinker* #{[1 2] [2 2] [3 2]})
(def *glider* #{[1 0] [2 1] [0 2] [1 2] [2 2]})
