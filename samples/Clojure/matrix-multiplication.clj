(defn transpose
  [s]
  (apply map vector s))

(defn nested-for
  [f x y]
  (map (fn [a]
         (map (fn [b]
                (f a b)) y))
       x))

(defn matrix-mult
  [a b]
  (nested-for (fn [x y] (reduce + (map * x y))) a (transpose b)))

(def ma [[1 1 1 1] [2 4 8 16] [3 9 27 81] [4 16 64 256]])
(def mb [[4 -3 4/3 -1/4] [-13/3 19/4 -7/3 11/24] [3/2 -2 7/6 -1/4] [-1/6 1/4 -1/6 1/24]])
