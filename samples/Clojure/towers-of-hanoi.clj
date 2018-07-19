(defn towers-of-hanoi [n from to via]
  (if (= n 1)
    (println (format "Move from %s to %s" from to))
    (do
      (towers-of-hanoi (dec n) from via to)
      (println (format "Move from %s to %s" from to))
      (recur (dec n) via to from))))
