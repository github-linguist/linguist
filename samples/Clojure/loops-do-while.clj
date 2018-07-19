(loop [i 0]
  (let [i* (inc i)]
    (println i*)
    (when-not (zero? (mod i* 6))
      (recur i*))))
