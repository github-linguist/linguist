(def ! (memoize #(apply * (range 1 (inc %)))))

(defn catalan-numbers-direct []
  (map #(/ (! (* 2 %))
	   (* (! (inc %)) (! %))) (range)))

(def catalan-numbers-recursive
     #(->> [1 1] ; [c0 n1]
	   (iterate (fn [[c n]]
		      [(* 2 (dec (* 2 n)) (/ (inc n)) c) (inc n)]) ,)
	   (map first ,)))

user> (take 15 (catalan-numbers-direct))
(1 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440)

user> (take 15 (catalan-numbers-recursive))
(1 1 2 5 14 42 132 429 1430 4862 16796 58786 208012 742900 2674440)
