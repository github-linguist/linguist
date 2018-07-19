;; provides floor and sqrt, but we use Java's sqrt as it's faster
;; (Clojure's is more exact)
(use 'clojure.contrib.math)


(defn nonsqr [#^Integer n] (+ n (floor (+ 0.5 (Math/sqrt n)))))
(defn square? [#^Double n]
  (let [r (floor (Math/sqrt n))]
    (= (* r r) n)))

(doseq [n (range 1 23)] (printf "%s -> %s\n" n (nonsqr n)))

(defn verify [] (not-any? square? (map nonsqr (range 1 1000000))) )
