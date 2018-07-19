(defn fac [n] (apply * (range 1 (inc n))))
(defmacro ct-factorial [n] (fac n))
