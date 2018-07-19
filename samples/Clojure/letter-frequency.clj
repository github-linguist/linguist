(println (sort-by second >
			(frequencies (map #(java.lang.Character/toUpperCase %)
					  (filter #(java.lang.Character/isLetter %) (slurp "text.txt"))))))
