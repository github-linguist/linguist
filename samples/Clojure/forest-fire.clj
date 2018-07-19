(def burn-prob 0.1)
(def new-tree-prob 0.5)

(defn grow-new-tree? [] (> new-tree-prob (rand)))
(defn burn-tree? [] (> burn-prob (rand)))
(defn tree-maker [] (if (grow-new-tree?) :tree :grass))

(defn make-forest
  ([] (make-forest 5))
  ([size]
  (take size (repeatedly #(take size (repeatedly tree-maker))))))

(defn tree-at [forest row col] (try (-> forest
                                   (nth row)
                                   (nth col))
                                    (catch Exception _ false)))

(defn neighbores-burning? [forest row col]
  (letfn [(burnt? [row col] (= :burnt (tree-at forest row col)))]
    (or
     (burnt? (inc row) col)
     (burnt? (dec row) col)
     (burnt? row (inc col))
     (burnt? row (dec col)))))

(defn lightning-strike [forest]
  (map (fn [forest-row]
         (map #(if (and (= % :tree) (burn-tree?))
                 :fire!
                 %)
              forest-row)
         )
       forest))

(defn burn-out-trees [forest]
  (map (fn [forest-row]
         (map #(case %
              :burnt :grass
              :fire! :burnt
              %)
              forest-row))
       forest))

(defn burn-neighbores [forest]
  (let [forest-size (count forest)
        indicies (partition forest-size (for [row (range forest-size) col (range forest-size)] (cons row (list col))))]
    (map (fn [forest-row indicies-row]
           (map #(if (and
                       (= :tree %)
                       (neighbores-burning? forest (first %2) (second %2)))
                    :fire!
                    %)
                forest-row indicies-row))
         forest indicies)))

(defn grow-new-trees [forest] (map (fn [forest-row]
                                     (map #(if (= % :grass)
                                             (tree-maker)
                                             %)
                                          forest-row))
                                     forest))

(defn forest-fire
  ([] (forest-fire 5))
  ([forest-size]
  (loop
      [forest (make-forest forest-size)]
    (pprint forest)
    (Thread/sleep 300)
    (-> forest
        (burn-out-trees)
        (lightning-strike)
        (burn-neighbores)
        (grow-new-trees)
        (recur)))))

(forest-fire)
