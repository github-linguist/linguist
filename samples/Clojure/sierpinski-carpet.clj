(ns example
  (:require [clojure.contrib.math :as math]))

(defn in-carpet? [x y]
  (loop [x x, y y]
    (cond
     (or (zero? x) (zero? y))              true
     (and (= 1 (mod x 3)) (= 1 (mod y 3))) false
     :else                                 (recur (quot x 3) (quot y 3)))))

(defn carpet [n]
  (apply str
         (interpose
	  \newline
	  (for [x (range (math/expt 3 n))]
	    (apply str
		   (for [y (range (math/expt 3 n))]
		     (if (in-carpet? x y) "*" " ")))))))

(println (carpet 3))
