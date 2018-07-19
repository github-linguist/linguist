(defn std-dev [samples]
  (let [n (count samples)
	mean (/ (reduce + samples) n)
	intermediate (map #(Math/pow (- %1 mean) 2) samples)]
    (Math/sqrt
     (/ (reduce + intermediate) n))))


(println (std-dev  [2 4 4 4 5 5 7 9])) ;;2.0
