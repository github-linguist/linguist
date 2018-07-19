(defn calc-pi [iterations]
  (loop [x (rand) y (rand) in 0 total 1]
    (if (< total iterations)
      (recur (rand) (rand) (if (<= (+ (* x x) (* y y)) 1) (inc in) in) (inc total))
      (double (* (/ in total) 4)))))

(doseq [x (take 5 (iterate #(* 10 %) 10))] (println (str (format "% 8d" x) ": " (calc-pi x))))
