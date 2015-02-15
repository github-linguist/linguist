(defn verify [rand n & [delta]]
  (let [rands (frequencies (repeatedly n rand))
        avg (/ (reduce + (map val rands)) (count rands))
        max-delta (* avg (or delta 1/10))
        acceptable? #(<= (- avg max-delta) % (+ avg max-delta))]
    (for [[num count] (sort rands)]
      [num count (acceptable? count)])))

(doseq [n [100 1000 10000]
        [num count okay?] (verify #(rand-int 7) n)]
  (println "Saw" num count "times:"
           (if okay? "that's" "   not") "acceptable"))
