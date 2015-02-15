(defn to-cdf [pdf]
  (reduce
    (fn [acc n] (conj acc (+ (or (last acc) 0) n)))
    []
    pdf))

(defn choose [cdf]
  (let [r (rand)]
    (count
      (filter (partial > r) cdf))))

(def *names* '[aleph beth gimel daleth he waw zayin heth])
(def *pdf* (map double [1/5 1/6 1/7 1/8 1/9 1/10 1/11 1759/27720]))

(let [num-trials 1000000
      cdf (to-cdf *pdf*)
      indexes (range (count *names*)) ;; use integer key internally, not name
      expected (into (sorted-map) (zipmap indexes *pdf*))
      actual (frequencies (repeatedly num-trials #(choose cdf)))]
  (doseq [[idx exp] expected]
    (println "Expected number of" (*names* idx) "was"
             (* num-trials exp) "and actually got" (actual idx))))
