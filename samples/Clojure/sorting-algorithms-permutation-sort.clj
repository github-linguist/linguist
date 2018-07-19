(use '[clojure.contrib.combinatorics :only (permutations)])

(defn permutation-sort [s]
  (first (filter (partial apply <=) (permutations s))))

(permutation-sort [2 3 5 3 5])
