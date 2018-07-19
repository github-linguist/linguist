(defn middle3 [v]
  (let [no (Math/abs v)
        digits (str no)
        len (count digits)]
    (cond
      (< len 3) :too_short
      (even? len) :no_middle_in_even_no_of_digits
      :else (let [mid (/ len 2)
                  start (- mid 2)]
              (apply str
                     (take 3
                           (nthnext digits start)))))))

(def passes '(123 12345 1234567 987654321 10001 -10001 -123 -100 100 -12345))
(def fails '(1 2 -1 -10 2002 -2002 0))
