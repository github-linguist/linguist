(defn compose [f g]
  (fn [x]
    (f (g x))))

; Example
(def inc2 (compose inc inc))
(println (inc2 5)) ; prints 7
