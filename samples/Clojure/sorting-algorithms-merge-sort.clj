(defn merge* [left right]
  (cond (nil? left) right
        (nil? right) left
        true (let [[l & *left] left
                   [r & *right] right]
               (if (<= l r) (cons l (merge* *left right))
                            (cons r (merge* left *right))))))

(defn merge-sort [L]
  (let [[l & *L] L]
    (if (nil? *L)
      L
      (let [[left right] (split-at (/ (count L) 2) L)]
        (merge* (merge-sort left) (merge-sort right))))))
