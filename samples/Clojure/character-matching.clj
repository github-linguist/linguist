(def evals '((. "abcd" startsWith "ab")
	     (. "abcd" endsWith "zn")
	     (. "abab" contains "bb")
	     (. "abab" contains "ab")
	     (. "abab" indexOf "bb")
	     (let [loc (. "abab" indexOf "ab")]
	       (. "abab" indexOf "ab" (dec loc)))))

user> (for [i evals] [i (eval i)])
([(. "abcd" startsWith "ab") true] [(. "abcd" endsWith "zn") false] [(. "abab" contains "bb") false] [(. "abab" contains "ab") true] [(. "abab" indexOf "bb") -1] [(let [loc (. "abab" indexOf "ab")] (. "abab" indexOf "ab" (dec loc))) 0])
