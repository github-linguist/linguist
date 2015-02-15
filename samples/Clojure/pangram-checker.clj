(defn pangram? [s]
  (let [letters (into #{} "abcdefghijklmnopqrstuvwxyz")]
    (= (->> s .toLowerCase (filter letters) (into #{})) letters)))
