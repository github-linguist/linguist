(ns pendulum
  (:import
    (javax.swing JFrame)
    (java.awt Canvas Graphics Color)))

(def length 200)
(def width (* 2 (+ 50 length)))
(def height (* 3 (/ length 2)))
(def dt 0.1)
(def g 9.812)
(def k (- (/ g length)))
(def anchor-x (/ width 2))
(def anchor-y (/ height 8))
(def angle (atom (/ (Math/PI) 2)))

(defn draw [#^Canvas canvas angle]
  (let [buffer  (.getBufferStrategy canvas)
        g       (.getDrawGraphics buffer)
        ball-x (+ anchor-x (* (Math/sin angle) length))
        ball-y (+ anchor-y (* (Math/cos angle) length))]
    (try
      (doto g
        (.setColor Color/BLACK)
        (.fillRect 0 0 width height)
        (.setColor Color/RED)
        (.drawLine anchor-x anchor-y ball-x ball-y)
        (.setColor Color/YELLOW)
        (.fillOval (- anchor-x 3) (- anchor-y 4) 7 7)
        (.fillOval (- ball-x 7) (- ball-y 7) 14 14))
      (finally (.dispose g)))
    (if-not (.contentsLost buffer)
      (.show buffer)) ))

(defn start-renderer [canvas]
  (->>
    (fn [] (draw canvas @angle) (recur))
    (new Thread)
    (.start)))

(defn -main [& args]
  (let [frame  (JFrame. "Pendulum")
        canvas (Canvas.)]

    (doto frame
      (.setSize width height)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setResizable false)
      (.add canvas)
      (.setVisible true))

    (doto canvas
      (.createBufferStrategy 2)
      (.setVisible true)
      (.requestFocus))

    (start-renderer canvas)

    (loop [v 0]
      (swap! angle #(+ % (* v dt)))
      (Thread/sleep 15)
      (recur (+ v (* k (Math/sin @angle) dt)))) ))

(-main)
