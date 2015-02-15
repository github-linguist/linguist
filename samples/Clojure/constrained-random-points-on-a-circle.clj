(import '[java.awt Color Graphics Dimension]
	'[javax.swing JFrame JPanel])

(let [points (->> (for [x (range -15 16)
			y (range -15 16)
			:when (<= 10 (Math/sqrt (+ (* x x) (* y y))) 15)]
		    [(+ x 15) (+ y 15)])
		  shuffle
		  (take 100 ,))]
  (doto (JFrame.)
    (.add (doto (proxy [JPanel] []
		  (paint [^Graphics g]
			 (doseq [[x y] points]
			   (.fillRect g (* 10 x) (* 10 y) 10 10))))
	    (.setPreferredSize (Dimension. 310 310))))
    (.setResizable false)
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
    .pack
    .show))
