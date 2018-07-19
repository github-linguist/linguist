(defn smerge [xs ys]
  (lazy-seq
    (let [x (first xs),
          y (first ys),
          [z xs* ys*]
          (cond
            (< x y) [x (rest xs) ys]
            (> x y) [y xs (rest ys)]
            :else   [x (rest xs) (rest ys)])]
      (cons z (smerge xs* ys*)))))

(defn smerge3 [xs ys zs]
  (smerge xs (smerge ys zs)))

(defn map*n [n ks] (map #(*' n %) ks))

(def hamming
  (lazy-seq
    (cons 1 (smerge3 (map*n 2 hamming) (map*n 3 hamming) (map*n 5 hamming)))))
