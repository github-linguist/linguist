(use '[clojure.contrib.lazy-seqs :only [primes]])

(defn lpf [n]
  [n (or (last
          (for [p (take-while #(<= (* % %) n) primes)
                :when (zero? (rem n p))]
            p))
         1)])

(->> (range 2 100000)
     (pmap lpf)
     (apply max-key second)
     println
     time)
