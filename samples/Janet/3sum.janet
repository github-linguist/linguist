(defn sum3
  "Solve the 3SUM problem in O(n^2) time."
  [s]
  (def tab @{})
  (def solutions @{})
  (def len (length s))
  (for k 0 len
    (put tab (s k) k))
  (for i 0 len
    (for j 0 len
      (def k (get tab (- 0 (s i) (s j))))
      (when (and k (not= k i) (not= k j) (not= i j))
        (put solutions {i true j true k true} true))))
  (map keys (keys solutions)))

(def arr @[2 4 1 3 8 7 -3 -1 12 -5 -8])
(printf "3sum of %P: " arr)
(printf "%P\n" (sum3 arr))
