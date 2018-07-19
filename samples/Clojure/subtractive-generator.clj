(defn xpat2-with-seed
  "produces an xpat2 function initialized from seed"
  [seed]
  (let [e9 1000000000
        fs (fn [[i j]] [j (mod (- i j) e9)])
        s (->> [seed 1] (iterate fs) (map first) (take 55) vec)
        rinit (map #(-> % inc (* 34) (mod 55) s) (range 55))
        r-atom (atom [54 (int-array rinit)])
        update (fn [[nprev r]]
                  (let [n (-> nprev inc (mod 55))
                        rx #(get r (-> n (- %) (mod 55)))
                        rn (-> (rx 55) (- (rx 24)) (mod e9))
                        _ (aset-int r n rn)]
                    [n r]))
        xpat2 #(let [[n r] (swap! r-atom update)]
                (get r n))
        _ (dotimes [_ 165] (xpat2))]
    xpat2))

(def xpat2 (xpat2-with-seed 292929))

(println (xpat2) (xpat2) (xpat2)) ; prints: 467478574 512932792 539453717
