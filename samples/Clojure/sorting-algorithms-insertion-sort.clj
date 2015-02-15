(defn in-sort! [data]
  (letfn [(insert ([raw x](insert [] raw x))
		  ([sorted [y & raw] x]
		     (if (nil? y) (conj sorted x)
			 (if (<= x y ) (concat sorted [x,y] raw)
			     (recur (conj sorted y)  raw x )))))]	
    (reduce insert [] data)))
;Usage:(in-sort! [6,8,5,9,3,2,1,4,7])
;Returns: [1 2 3 4 5 6 7 8 9]
