(defn draw-line
  "Draw a line from x1,y1 to x2,y2 using Bresenham's, to a java BufferedImage in the colour of pixel."
  [buffer x1 y1 x2 y2 pixel]
  (let [dist-x (abs (- x1 x2))
   dist-y (abs (- y1 y2))
   steep (> dist-y dist-x)]
    (let [[x1 y1 x2 y2] (if steep [y1 x1 y2 x2] [x1 y1 x2 y2])]
      (let [[x1 y1 x2 y2] (if (> x1 x2) [x2 y2 x1 y1] [x1 y1 x2 y2])]
  (let  [delta-x (- x2 x1)
     delta-y (abs (- y1 y2))
     y-step (if (< y1 y2) 1 -1)]

    (let [plot (if steep
       #(.setRGB buffer (int %1) (int %2) pixel)
       #(.setRGB buffer (int %2) (int %1) pixel))]

      (loop [x x1 y y1 error (floor delta-x 2) ]
        (plot x y)
        (if (< x x2)
    ; Rather then rebind error, test that it is less than delta-y rather than zero
    (if (< error delta-y)
      (recur (inc x) (+ y y-step) (+ error (- delta-x delta-y)))
      (recur (inc x) y            (- error delta-y)))))))))))
