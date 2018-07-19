(defn partitions [sizes coll]
  (lazy-seq
   (when-let [n (first sizes)]
     (when-let [s (seq coll)]
       (cons (take n coll)
	     (partitions (next sizes) (drop n coll)))))))

(defn take-from [n colls]
  (lazy-seq
   (when-let [s (seq colls)]
     (let [[first-n rest-n] (split-at n s)]
       (cons (map first first-n)
	     (take-from n (concat (filter seq (map rest first-n)) rest-n)))))))

(defn zig-zag [n]
  (->> (partitions (concat (range 1 (inc n)) (range (dec n) 0 -1)) (range (* n n)))
       (map #(%1 %2) (cycle [reverse identity]) ,)
       (take-from n ,)))

user> (zig-zag 5)
(( 0  1  5  6 14)
 ( 2  4  7 13 15)
 ( 3  8 12 16 21)
 ( 9 11 17 20 22)
 (10 18 19 23 24))

user> (zig-zag 6)
(( 0  1  5  6 14 15)
 ( 2  4  7 13 16 25)
 ( 3  8 12 17 24 26)
 ( 9 11 18 23 27 32)
 (10 19 22 28 31 33)
 (20 21 29 30 34 35))
