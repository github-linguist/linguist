(defn- digit-to-num [d] (Character/digit d 10))
(defn- square [n] (* n n))

(defn happy? [n]
  (loop [n n, seen #{}]
    (cond (= n 1)  true
          (seen n) false
          :else
          (recur (reduce + (map (comp square digit-to-num) (str n)))
                 (conj seen n)))))

(def happy-numbers (filter happy? (iterate inc 1)))

(println (take 8 happy-numbers))
