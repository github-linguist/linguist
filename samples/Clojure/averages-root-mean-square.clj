(use '[clojure.contrib.math :only (sqrt)])

(defn rms [xs]
  (sqrt (/ (reduce + (map #(* % %) xs))
	   (count xs))))

(println (rms (range 1 11)))
