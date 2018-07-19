(defn levenshtein [str1 str2]
  (let [len1 (count str1)
        len2 (count str2)]
    (cond (zero? len1) len2
          (zero? len2) len1
          :else
          (let [cost (if (= (first str1) (first str2)) 0 1)]
            (min (inc (levenshtein (rest str1) str2))
                 (inc (levenshtein str1 (rest str2)))
                 (+ cost
                    (levenshtein (rest str1) (rest str2))))))))

(println (levenshtein "rosettacode" "raisethysword"))
