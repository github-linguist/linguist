(def size 8)

(defn extends? [v n]
  (let [k (count v)]
    (not-any? true?
      (for [i (range k) :let [vi (v i)]]
        (or
          (= vi n)  ;check for shared row
          (= (- k i) (Math/abs (- n vi)))))))) ;check for shared diagonal

(defn extend [vs]
  (for [v vs
        n (range 1 (inc size)) :when (extends? v n)]
    (conj v n)))


(def solutions
  (nth (iterate extend [[]]) size))

(doseq [s solutions]
  (println s))

(println (count solutions) "solutions")
