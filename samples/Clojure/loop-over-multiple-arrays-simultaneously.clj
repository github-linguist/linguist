(doseq [s (map #(str %1 %2 %3) "abc" "ABC" "123")]
  (println s))
