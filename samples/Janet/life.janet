# A game of life implementation

(def- window
  (seq [x :range [-1 2]
         y :range [-1 2]
         :when (not (and (zero? x) (zero? y)))]
       [x y]))

(defn- neighbors
  [[x y]]
  (map (fn [[x1 y1]] [(+ x x1) (+ y y1)]) window))

(defn tick
  "Get the next state in the Game Of Life."
  [state]
  (def cell-set (frequencies state))
  (def neighbor-set (frequencies (mapcat neighbors state)))
  (seq [coord :keys neighbor-set
         :let [count (get neighbor-set coord)]
         :when (or (= count 3) (and (get cell-set coord) (= count 2)))]
      coord))

(defn draw
  "Draw cells in the game of life from (x1, y1) to (x2, y2)"
  [state x1 y1 x2 y2]
  (def cellset @{})
  (each cell state (put cellset cell true))
  (loop [x :range [x1 (+ 1 x2)]
         :after (print)
         y :range [y1 (+ 1 y2)]]
    (file/write stdout (if (get cellset [x y]) "X " ". ")))
  (print))

#
# Run the example
#

(var *state* '[(0 0) (-1 0) (1 0) (1 1) (0 2)])

(for i 0 20
  (print "generation " i)
  (draw *state* -7 -7 7 7)
  (set *state* (tick *state*)))
