(ns nested)

(defn create-matrix [width height]
  (for [_ (range width)]
    (for [_ (range height)]
      (inc (rand-int 20)))))

(defn print-matrix [matrix]
  (loop [[row & rs] matrix]
    (when (= (loop [[x & xs] row]
               (println x)
               (cond (= x 20) :stop
                     xs (recur xs)
                     :else :continue))
             :continue)
      (when rs (recur rs)))))

(print-matrix (create-matrix 10 10))
