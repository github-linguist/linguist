(use 'clojure.contrib.combinatorics)

(defn nested-replace [l m]
(cond
(= l '()) '()
(m (first l)) (concat (list (m (first l))) (nested-replace (rest l) m))
(seq? (first l)) (concat (list (nested-replace (first l) m)) (nested-replace (rest l) m))
true (concat (list (first l)) (nested-replace (rest l) m))))

(defn format-solution [sol]
(cond
(number? sol) sol
(seq? sol)
    (list (format-solution (second sol)) (first sol) (format-solution (nth sol 2)))))

(defn play24 [& digits] (count (map #(-> % format-solution println)
(let [operator-map-list (map (fn [a] {:op1 (nth a 0) :op2 (nth a 1) :op3 (nth a 2)})
       (selections '(* + - /) 3))
     digits-map-list
       (map (fn [a] {:num1 (nth a 0) :num2 (nth a 1) :num3 (nth a 2) :num4 (nth a 3)})
         (permutations digits))
     patterns-list (list
       '(:op1 (:op2 :num1 :num2) (:op3 :num3 :num4))
       '(:op1 :num1 (:op2 :num2 (:op3 :num3 :num4))))
       ;other patterns can be added here, e.g. '(:op1 (:op2 (:op3 :num1 :num2) :num3) :num4)
     op-subbed (reduce concat '()
       (map (fn [a] (map #(nested-replace a % ) operator-map-list)) patterns-list))
     full-subbed (reduce concat '()
       (map (fn [a] (map #(nested-replace % a) op-subbed)) digits-map-list))]
     (filter #(= (try (eval %) (catch Exception e nil)) 24) full-subbed)))))
