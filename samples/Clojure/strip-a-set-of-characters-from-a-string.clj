(defn strip [coll chars]
  (apply str (remove #((set chars) %) coll)))

(strip "She was a soul stripper. She took my heart!" "aei")
;; => "Sh ws  soul strppr. Sh took my hrt!"
