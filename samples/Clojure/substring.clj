(def string "alphabet")
(def n 2)
(def m 4)
(def len (count string))

;starting from n characters in and of m length;
(println
 (subs string n (+ n m)))              ;phab
;starting from n characters in, up to the end of the string;
(println
 (subs string n))                      ;phabet
;whole string minus last character;
(println
 (subs string 0 (dec len)))            ;alphabe
;starting from a known character within the string and of m length;
(let [pos (.indexOf string (int \l))]
  (println
   (subs string pos (+ pos m))))     ;lpha
;starting from a known substring within the string and of m length.
(let [pos (.indexOf string "ph")]
  (println
   (subs string pos (+ pos m))))      ;phab
