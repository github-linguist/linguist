(ns mandelbrot
  (:refer-clojure :exclude [+ * <])
  (:use (clojure.contrib complex-numbers)
        (clojure.contrib.generic [arithmetic :only [+ *]]
                                 [comparison :only [<]]
                                 [math-functions :only [abs]])))
(defn mandelbrot? [z]
  (loop [c 1
         m (iterate #(+ z (* % %)) 0)]
    (if (and (> 20 c)
             (< (abs (first m)) 2) )
      (recur (inc c)
             (rest m))
      (if (= 20 c) true false))))

(defn mandelbrot []
  (for [y (range 1 -1 -0.05)
	x (range -2 0.5 0.0315)]
    (if (mandelbrot? (complex x y)) "#" " ")))

(println (interpose \newline (map #(apply str %) (partition 80 (mandelbrot)))))
