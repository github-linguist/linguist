(defn fwd-diff [nums order]
  (nth (iterate #(map - (next %) %) nums) order))
