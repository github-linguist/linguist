(use '[clojure.contrib.math :only (expt)])

(defn a-mean [coll]
  (/ (apply + coll) (count coll)))

(defn g-mean [coll]
  (expt (apply * coll) (/ (count coll))))

(defn h-mean [coll]
  (/ (count coll) (apply + (map / coll))))

(let [numbers (range 1 11)
      a (a-mean numbers) g (g-mean numbers) h (h-mean numbers)]
  (println a ">=" g ">=" h)
  (>= a g h))
