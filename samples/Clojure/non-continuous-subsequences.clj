(use '[clojure.contrib.combinatorics :only (subsets)])

(defn of-min-length [min-length]
  (fn [s] (>= (count s) min-length)))

(defn runs [c l]
  (map (partial take l) (take-while not-empty (iterate rest c))))

(defn is-subseq? [c sub]
  (some identity (map = (runs c (count sub)) (repeat sub))))

(defn non-continuous-subsequences [s]
  (filter (complement (partial is-subseq? s)) (subsets s)))


(filter (of-min-length 2) (non-continuous-subsequences [:a :b :c :d]))
