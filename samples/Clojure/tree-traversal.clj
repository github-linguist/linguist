(defn walk [node f order]
  (when node
   (doseq [o order]
     (if (= o :visit)
       (f (:val node))
       (walk (node o) f order)))))

(defn preorder [node f]
  (walk node f [:visit :left :right]))

(defn inorder [node f]
  (walk node f [:left :visit :right]))

(defn postorder [node f]
  (walk node f [:left :right :visit]))

(defn queue [& xs]
  (when (seq xs)
   (apply conj clojure.lang.PersistentQueue/EMPTY xs)))

(defn level-order [root f]
  (loop [q (queue root)]
    (when-not (empty? q)
      (if-let [node (first q)]
        (do
          (f (:val node))
          (recur (conj (pop q) (:left node) (:right node))))
        (recur (pop q))))))

(defn vec-to-tree [t]
  (if (vector? t)
    (let [[val left right] t]
      {:val val
       :left (vec-to-tree left)
       :right (vec-to-tree right)})
    t))

(let [tree (vec-to-tree [1 [2 [4 [7]] [5]] [3 [6 [8] [9]]]])
      fs   '[preorder inorder postorder level-order]
      pr-node #(print (format "%2d" %))]
  (doseq [f fs]
    (print (format "%-12s" (str f ":")))
    ((resolve f) tree pr-node)
    (println)))
