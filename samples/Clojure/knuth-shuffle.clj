(defn shuffle [vect]
  (reduce (fn [v i] (let [r (rand-int i)]
                      (assoc v i (v r) r (v i)))
          vect (range (dec (count vect)) 1 -1)))
