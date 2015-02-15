; a set of keywords
(def fruits #{:apple :banana :cherry})

; a predicate to test "fruit" membership
(defn fruit? [x] (contains? fruits x))

; if you need a value associated with each fruit
(def fruit-value (zipmap fruits (iterate inc 1)))

(println (fruit? :apple))
(println (fruit-value :banana))
