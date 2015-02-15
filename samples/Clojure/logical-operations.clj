(defn logical [a b]
  (prn (str "a and b is " (and a b)))
  (prn (str "a or b is " (or a b)))
  (prn (str "not a is "  (not a))))

(logical true false)
