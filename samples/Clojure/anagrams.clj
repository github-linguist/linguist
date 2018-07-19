(require '[clojure.java.io :as io])

(def groups
  (with-open [r (io/reader wordfile)]
    (group-by sort (line-seq r))))

(let [wordlists (sort-by (comp - count) (vals groups))
      maxlength (count (first wordlists))]
  (doseq [wordlist (take-while #(= (count %) maxlength) wordlists)]
    (println wordlist))
