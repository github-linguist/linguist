(defn fib [n]
  (when (neg? n)
    (throw (new IllegalArgumentException "n should be > 0")))
  (loop [n n, v1 1, v2 1]
    (if (< n 2)
      v2
      (recur (dec n) v2 (+ v1 v2)))))
