(defn take-random [n coll]
  (->> (repeatedly #(rand-nth coll))
       distinct
       (take n ,)))

(defn postwalk-fs
  "Depth first post-order traversal of form, apply successive fs at each level.
  (f1 (map f2 [..]))"
  [[f & fs] form]
  (f
   (if (and (seq fs) (coll? form))
     (into (empty form) (map (partial postwalk-fs fs) form))
     form)))

(defn neighbors [x y n m pred]
  (for [dx (range (Math/max 0 (dec x)) (Math/min n (+ 2 x)))
	dy (range (Math/max 0 (dec y)) (Math/min m (+ 2 y)))
	:when (pred dx dy)]
    [dx dy]))

(defn new-game [n m density]
  (let [mines (set (take-random (Math/floor (* n m density)) (range (* n m))))]
    (->> (for [y (range m)
	       x (range n)
	       :let [neighbor-mines (count (neighbors x y n m #(mines (+ %1 (* %2 n)))))]]
	   (#(if (mines (+ (* y n) x)) (assoc % :mine true) %) {:value neighbor-mines}))
	 (partition n ,)
	 (postwalk-fs [vec vec] ,))))

(defn display [board]
  (postwalk-fs [identity println #(condp % nil
				    :marked \?
				    :opened (:value %)
				    \.)] board))

(defn boom [{board :board}]
  (postwalk-fs [identity println #(if (:mine %) \* (:value %))] board)
  true)

(defn open* [board [[x y] & rest]]
  (if-let [value (get-in board [y x :value])] ; if nil? value -> nil? x -> nil? queue
    (recur
     (assoc-in board [y x :opened] true)
     (if (pos? value)
       rest
       (concat rest
	       (neighbors x y (count (first board)) (count board)
			  #(not (get-in board [%2 %1 :opened]))))))
    board))

(defn open [board x y]
  (let [x (dec x), y (dec y)]
    (condp (get-in board [y x]) nil
      :mine {:boom true :board board}
      :opened board
      (open* board [[x y]]))))

(defn mark [board x y]
  (let [x (dec x), y (dec y)]
    (assoc-in board [y x :marked] (not (get-in board [y x :marked])))))

(defn done? [board]
  (if (:boom board)
    (boom board)
    (do (display board)
	(->> (flatten board)
	     (remove :mine ,)
	     (every? :opened ,)))))

(defn play [n m density]
  (let [board (new-game n m density)]
    (println [:mines (count (filter :mine (flatten board)))])
    (loop [board board]
      (when-not (done? board)
	(print ">")
	(let [[cmd & xy] (.split #" " (read-line))
	      [x y] (map #(Integer. %) xy)]
	  (recur ((if (= cmd "mark") mark open) board x y)))))))
