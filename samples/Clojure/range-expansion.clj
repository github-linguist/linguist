(defn split [s sep]
      (defn skipFirst [[x & xs :as s]]
	(cond (empty? s) [nil nil]
	      (= x sep)  [x xs]
	      true       [nil s]))
      (loop [lst '(), s s]
	 (if (empty? s) (reverse lst)
	     (let [[hd trunc] (skipFirst s)
	           [word news] (split-with #(not= % sep) trunc)
		   cWord (cons hd word)]
		     (recur (cons (apply str cWord) lst)
		      	  (apply str (rest news)))))))

(defn parseRange [[x & xs :as s]]
       (if (some #(= % \-) xs)
	   (let [[r0 r1] (split s \-)]
		(range (read-string r0) (inc (read-string r1))))
	   (list (read-string (str s))))))

(defn rangeexpand [s]
  (flatten (map parseRange (split s \,))))

> (rangeexpand "-6,-3--1,3-5,7-11,14,15,17-20")
(-6 -3 -2 -1 3 4 5 7 8 9 10 11 14 15 17 18 19 20)
