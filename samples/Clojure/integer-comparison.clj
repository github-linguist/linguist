(let [[a b] (repeatedly read)]
  (doseq [[op string] [[< "less than"]
                       [> "greater than"]
                       [= "equal to"]]]
    (when (op a b)
      (println (str a " is " string " " b)))))
