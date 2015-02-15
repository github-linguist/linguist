(defn iterator [a b]
  (fn[x] (mod (+ (* a x) b) (bit-shift-left 1 31))))

(def bsd (drop 1 (iterate (iterator 1103515245 12345) 0)))

(def ms (drop 1 (for [x (iterate  (iterator 214013 2531011) 0)] (bit-shift-right x 16))))

(take 10 bsd) ;-> (12345 1406932606 654583775 1449466924 229283573 1109335178 1051550459 1293799192 794471793 551188310)
(take 10 ms) ;-> (38 7719 21238 2437 8855 11797 8365 32285 10450 30612)
