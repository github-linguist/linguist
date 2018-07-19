(ns bogosort
  (:use [clojure.contrib.seq-utils :only (shuffle)]))

(defn in-order? [less xs]
  (or (empty? xs)
      (empty? (next xs))
      (and (less (first xs) (second xs))
           (recur less (next xs)))))

(defn bogosort
  ([xs]
     (bogosort < xs))
  ([less xs]
     (if (in-order? less xs) xs
	 (recur less (shuffle xs)))))

(println (bogosort [7,5,12,1,4,2,23,18]))
