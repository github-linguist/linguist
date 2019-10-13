# Find the maximum path from the top (root)
# of the triangle to the leaves of the triangle.

(defn myfold [xs ys]
  (let [m1 (map + [;xs 0] ys)
        m2 (map + [0 ;xs] ys)]
    (map max m1 m2)))

(defn maxpath [t]
  (extreme > (reduce myfold () t)))

# Test it
# Maximum path is 3 -> 10 -> 3 -> 9 for a total of 25

(def triangle 
  '[[3]
    [7 10]
    [4 3 7]
    [8 9 1 3]])

(print (maxpath triangle))
