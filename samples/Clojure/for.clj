(defn prime? [n]
   (not-any? zero? (map #(rem n %) (range 2 n))))

(range 3 33 2)
'(3 5 7 9 11 13 15 17 19 21 23 25 27 29 31)

;; :when continues through the collection even if some have the
;; condition evaluate to false, like filter
(for [x (range 3 33 2) :when (prime? x)]
   x)
'(3 5 7 11 13 17 19 23 29 31)

;; :while stops at the first collection element that evaluates to
;; false, like take-while
(for [x (range 3 33 2) :while (prime? x)]
   x)
'(3 5 7)
