(let [rows (Integer/parseInt (read-line))
      cols (Integer/parseInt (read-line))
      a (to-array-2d (repeat rows (repeat cols nil)))]
  (aset a 0 0 12)
  (println "Element at 0,0:" (aget a 0 0)))
