(use '[clojure.set])

(defn symmetric-difference [s1 s2]
  (union (difference s1 s2) (difference s2 s1)))

(symmetric-difference #{:john :bob :mary :serena} #{:jim :mary :john :bob})
