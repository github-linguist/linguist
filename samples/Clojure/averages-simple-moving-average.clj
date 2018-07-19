(import '[clojure.lang PersistentQueue])

(defn enqueue-max [q p n]
  (let [q (conj q n)]
    (if (<= (count q) p) q (pop q))))

(defn avg [coll] (/ (reduce + coll) (count coll)))

(defn init-moving-avg [p]
  (let [state (atom PersistentQueue/EMPTY)]
    (fn [n]
      (avg (swap! state enqueue-max p n)))))
