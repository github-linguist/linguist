(defn maxcat [coll]
  (read-string
    (apply str
           (sort (fn [x y]
                   (apply compare
                          (map read-string [(str y x) (str x y)])))
                 coll))))

(prn (map maxcat [[1 34 3 98 9 76 45 4] [54 546 548 60]]))
