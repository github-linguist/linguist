(defn halve [n]
  (bit-shift-right n 1))

(defn twice [n]          ; 'double' is taken
  (bit-shift-left n 1))

(defn even [n]           ; 'even?' is the standard fn
  (zero? (bit-and n 1)))

(defn emult [x y]
  (reduce +
    (map second
      (filter #(not (even (first %))) ; a.k.a. 'odd?'
        (take-while #(pos? (first %))
          (map vector
            (iterate halve x)
            (iterate twice y)))))))

(defn emult2 [x y]
  (loop [a x, b y, r 0]
    (if (= a 1)
      (+ r b)
      (if (even a)
        (recur (halve a) (twice b) r)
        (recur (halve a) (twice b) (+ r b))))))
