(defn digsum [n acc]
  (if (zero? n) acc
      (digsum (quot n 10) (+ acc (mod n 10)))))

(let [harshads (filter
                 #(zero? (mod % (digsum % 0)))
                 (iterate inc 1))]
  (prn (take 20 harshads))
  (prn (first (filter #(> % 1000) harshads))))
