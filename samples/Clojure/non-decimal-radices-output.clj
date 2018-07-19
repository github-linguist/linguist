(Integer/toBinaryString 25) ; returns "11001"
(Integer/toOctalString 25)  ; returns "31"
(Integer/toHexString 25)    ; returns "19"

(dotimes [i 20]
  (println (Integer/toHexString i)))
