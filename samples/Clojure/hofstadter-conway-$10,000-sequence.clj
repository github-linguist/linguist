(use 'clojure.contrib.math) ; for expt

;; A literal transcription of the definition, with memoize doing the heavy lifting
(def conway
     (memoize
      (fn [x]
        (if (< x 3)
          1
          (+ (conway (conway (dec x)))
             (conway (- x (conway (dec x)))))))))

(def N (drop 1 (range))) ; natural numbers

;; This is enough to compute them all. The rest is grouping and max-finding
(def all-conways (map conway N))

;; All the powers of two
(def pow2 (map #(expt 2 %) N))

;; Find the lowest power of two > n
(defn lowest-pow-higher [n]
  (some #(and (> % n) %) pow2))

;; Split the natural numbers into groups at each power of two
(def groups (partition-by lowest-pow-higher N))

;; The conway numbers of each number in the group
(def C (map #(map conway %) groups))

;; Each conway number divided by its index
(def ratios (map #(map / %1 %2) C groups))

;; The largest value in each group of ratios
(def maxima (map #(apply max %) ratios))

(take 4 maxima) ; no rounding errors: still using ratios
;; yields (1 2/3 2/3 7/11)

(def first-20 (take 20 maxima))
(apply >= first-20) ; yields true - the sequence is decreasing
(map double first-20) ; to get them in decimal form
