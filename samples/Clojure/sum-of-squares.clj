(defn sum-of-squares [v]
  (reduce #(+ %1 (* %2 %2)) 0 v))
