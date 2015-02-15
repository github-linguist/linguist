(defn comment-strip [txt & args]
  (let [args (conj {:delim ["/*" "*/"]} (apply hash-map args)) ; This is the standard way of doing keyword/optional arguments in Clojure
	[opener closer] (:delim args)]
    (loop [out "", txt txt, delim-count 0] ; delim-count is needed to handle nested comments
      (let [[hdtxt resttxt] (split-at (count opener) txt)] ; This splits "/* blah blah */" into hdtxt="/*" and restxt="blah blah */"	
	(printf "hdtxt=%8s resttxt=%8s out=%8s txt=%16s delim-count=%s\n" (apply str hdtxt) (apply str resttxt) out (apply str txt) delim-count)
	(cond
	 (empty? hdtxt)    (str out (apply str txt))
	 (= (apply str hdtxt) opener) (recur out resttxt (inc delim-count))
	 (= (apply str hdtxt) closer) (recur out resttxt (dec delim-count))
	 (= delim-count 0)(recur (str out (first txt)) (rest txt) delim-count)
	 true             (recur out (rest txt) delim-count))))))
