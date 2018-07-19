(defn gen-brackets [n]
  (->> (concat (repeat n \[) (repeat n \]))
       shuffle
       (apply str ,)))

(defn balanced? [s]
  (loop [[first & coll] (seq s)
	 stack '()]
    (if first
      (if (= first \[)
	(recur coll (conj stack \[))
	(when (= (peek stack) \[)
	  (recur coll (pop stack))))
      (zero? (count stack)))))
