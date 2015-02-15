(defn score [before after]
   (->> (map = before after)
	(filter true? ,)
	count))

(defn merge-vecs [init vecs]
  (reduce (fn [counts [index x]]
		 (assoc counts x (conj (get counts x []) index)))
	  init vecs))

(defn frequency
  "Returns a collection of indecies of distinct items"
  [coll]
  (->> (map-indexed vector coll)
       (merge-vecs {} ,)))

(defn group-indecies [s]
  (->> (frequency s)
       vals
       (sort-by count ,)
       reverse))

(defn cycles [coll]
  (let [n (count (first coll))
	cycle (cycle (range n))
	coll (apply concat coll)]
    (->> (map vector coll cycle)
	 (merge-vecs [] ,))))

(defn rotate [n coll]
  (let [c (count coll)
	n (rem (+ c n) c)]
    (concat (drop n coll) (take n coll))))

(defn best-shuffle [s]
  (let [ref (cycles (group-indecies s))
	prm (apply concat (map (partial rotate 1) ref))
	ref (apply concat ref)]
    (->> (map vector ref prm)
	 (sort-by first ,)
	 (map second ,)
	 (map (partial get s) ,)
	 (apply str ,)
	 (#(vector s % (score s %))))))

user> (->> ["abracadabra" "seesaw" "elk" "grrrrrr" "up" "a"]
	   (map best-shuffle ,)
	   vec)
[["abracadabra" "bdabararaac" 0]
 ["seesaw" "eawess" 0]
 ["elk" "lke" 0]
 ["grrrrrr" "rgrrrrr" 5]
 ["up" "pu" 0]
 ["a" "a" 1]]
