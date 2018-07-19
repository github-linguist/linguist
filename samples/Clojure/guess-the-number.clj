(def target (inc (rand-int 10))

(loop [n 0]
   (println "Guess a number between 1 and 10 until you get it right:")
   (let [guess (read)]
	(if (= guess target)
	    (printf "Correct on the %d guess.\n" n)
	    (do
	     (println "Try again")
	     (recur (inc n))))))
