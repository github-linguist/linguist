(defn nacci [init]
  (letfn [(s [] (lazy-cat init (apply map + (map #(drop % (s)) (range (count init))))))]
    (s)))

(let [show (fn [name init] (println "first 20" name (take 20 (nacci init))))]
  (show "Fibonacci" [1 1])
  (show "Tribonacci" [1 1 2])
  (show "Tetranacci" [1 1 2 4])
  (show "Lucas" [2 1]))
