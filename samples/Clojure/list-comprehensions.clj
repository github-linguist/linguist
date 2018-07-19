(defn pythagorean-triples [n]
  (for [x (range 1 (inc n))
	y (range x (inc n))
	z (range y (inc n))
	:when (= (+ (* x x) (* y y)) (* z z))]
    [x y z]))
