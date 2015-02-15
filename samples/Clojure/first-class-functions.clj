(use 'clojure.contrib.math)
(let [fns [#(Math/sin %) #(Math/cos %) (fn [x] (* x x x))]
      inv [#(Math/asin %) #(Math/acos %) #(expt % 1/3)]]
  (map #(% 0.5) (map #(comp %1 %2) fns inv)))
