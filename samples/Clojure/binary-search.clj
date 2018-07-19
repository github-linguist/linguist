(defn bsearch
  ([coll t]
    (bsearch coll 0 (dec (count coll)) t))
  ([coll l u t]
    (if (> l u) -1
      (let [m (quot (+ l u) 2) mth (nth coll m)]
        (cond
          ; the middle element is greater than t
          ; so search the lower half
          (> mth t) (recur coll l (dec m) t)
          ; the middle element is less than t
          ; so search the upper half
          (< mth t) (recur coll (inc m) u t)
          ; we've found our target
          ; so return its index
          (= mth t) m)))))
