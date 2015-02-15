(defn guess-run []
  (let [start 1
	end 100
	target (+ start (rand-int (inc (- end start))))]
    (printf "Guess a number between %d and %d" start end)
    (loop [i 1]
      (printf "Your guess %d:\n" i)
      (let [ans (read)]
	(if (cond
	     (not (number? ans)) (println "Invalid format")
	     (or (< ans start) (> ans end)) (println "Out of range")
	     (< ans target)    (println "too low")
	     (> ans target)    (println "too high")
	     :else             true)
	  (println "Correct")
	  (recur (inc i)))))))
