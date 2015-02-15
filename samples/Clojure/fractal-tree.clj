(import '[java.awt Color Graphics]
	'javax.swing.JFrame)

(defn deg-to-radian [deg] (* deg Math/PI 1/180))
(defn cos-deg [angle] (Math/cos (deg-to-radian angle)))
(defn sin-deg [angle] (Math/sin (deg-to-radian angle)))

(defn draw-tree [^Graphics g, x y angle depth]
  (when (pos? depth)
    (let [x2 (+ x (int (* depth 10 (cos-deg angle))))
	  y2 (+ y (int (* depth 10 (sin-deg angle))))]
      (.drawLine g x y x2 y2)
      (draw-tree g x2 y2 (- angle 20) (dec depth))
      (recur     g x2 y2 (+ angle 20) (dec depth)))))

(defn fractal-tree [depth]
  (doto (proxy [JFrame] []
	  (paint [g]
		 (.setColor g Color/BLACK)
		 (draw-tree g 400 500 -90 depth)))
    (.setBounds 100 100 800 600)
    (.setResizable false)
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
    (.show)))

(fractal-tree 9)
