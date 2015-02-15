(defn hailstone-seq [n]
  {:pre [(pos? n)]}
  (lazy-seq
   (cond (= n 1)   '(1)
         (even? n) (cons n (hailstone-seq (/ n 2)))
         :else     (cons n (hailstone-seq (+ (* n 3) 1))))))

(let [hseq (hailstone-seq 27)]
  (->  hseq count      (= 112)            assert)
  (->> hseq (take 4)   (= [27 82 41 124]) assert)
  (->> hseq (drop 108) (= [8 4 2 1])      assert))

(let [{max-i :num, max-len :len}
      (reduce #(max-key :len %1 %2)
              (for [i (range 1 100000)]
                {:num i, :len (count (hailstone-seq i))}))]
  (println "Maximum length" max-len "was found for hailstone(" max-i ")."))
