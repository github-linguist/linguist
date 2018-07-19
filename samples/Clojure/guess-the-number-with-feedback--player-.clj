(require '[clojure.string :as str])

(defn guess-game [low high]
  (printf "Think of a number between %s and %s.\n (use (h)igh (l)ow (c)orrect)\n" low high)
  (loop [guess (/ (inc (- high low)) 2)
         [step & more] (next (iterate #(/ % 2) guess))]
    (printf "I guess %s\n=> " (Math/round (float guess)))
    (flush)
    (case (first (str/lower-case (read)))
      \h (recur (- guess step) more)
      \l (recur (+ guess step) more)
      \c (println "Huzzah!")
      (do (println "Invalid input.")
          (recur guess step)))))
