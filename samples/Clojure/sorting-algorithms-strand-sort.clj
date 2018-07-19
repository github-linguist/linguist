(ns rosettacode.strand-sort)

(defn merge-join
  "Produces a globally sorted seq from two sorted seqables"
  [[a & la :as all] [b & lb :as bll]]
  (cond (nil? a) bll
        (nil? b) all
        (< a b) (cons a (lazy-seq (merge-join la bll)))
        true    (cons b (lazy-seq (merge-join all lb)))))

(defn unbraid
  "Separates a sorted list from a sequence"
  [u]
  (when (seq u)
    (loop [[x & xs] u
           u []
           s []
           e x]
      (if (nil? x)
        [s u]
        (if (>= x e)
          (recur xs u (conj s x) x)
          (recur xs (conj u x) s e))))))

(defn strand-sort
  "http://en.wikipedia.org/wiki/Strand_sort"
  [s]
  (loop [[s u] (unbraid s)
         m nil]
    (if s
      (recur (unbraid u) (merge-join m s))
      m)))

(strand-sort [1, 6, 3, 2, 1, 7, 5, 3])
;;=> (1 1 2 3 3 5 6 7)
