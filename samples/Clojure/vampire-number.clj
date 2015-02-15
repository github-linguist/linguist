(defn factor-pairs [n]
  (for [x (range 2 (Math/sqrt n))
        :when (zero? (mod n x))]
    [x (quot n x)]))

(defn fangs [n]
  (let [dlen (comp count str)
        half (/ (dlen n) 2)
        halves? #(apply = (cons half (map dlen %)))
        digits  #(sort (apply str %))]
    (filter #(and (halves? %)
                  (= (sort (str n)) (digits %)))
            (factor-pairs n))))

(defn vampiric? [n]
  (let [fangs (fangs n)]
    (if (empty? fangs) nil [n fangs])))

(doseq [n (take 25 (keep vampiric? (range)))]
  (prn n))

(doseq [n [16758243290880, 24959017348650, 14593825548650]]
  (println (or (vampiric? n) (str n " is not vampiric."))))
