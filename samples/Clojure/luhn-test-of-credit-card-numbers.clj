(defn- digits [n]
  (map #(Character/digit % 10) (str n)))

(defn luhn? [n]
  (let [sum (reduce + (map
                       (fn [d idx]
                         (if (even? idx)
                           (reduce + (digits (* d 2)))
                           d))
                       (reverse (digits n))
                       (iterate inc 1)))]
    (zero? (mod sum 10))))

(doseq [n [49927398716 49927398717 1234567812345678 1234567812345670]]
  (println (luhn? n)))
