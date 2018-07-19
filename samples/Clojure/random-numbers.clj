(import '(java.util Random))
(def normals
  (let [r (Random.)]
    (take 1000 (repeatedly #(-> r .nextGaussian (* 0.5) (+ 1.0))))))
