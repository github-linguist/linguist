(import '[javax.swing JFrame JLabel])
(import '[java.awt.event MouseAdapter])

(def text "Hello World! ")
(def text-ct (count text))
(def rotations
  (vec
    (take text-ct
      (map #(apply str %)
        (partition text-ct 1 (cycle text))))))

(def pos (atom 0))  ;position in rotations vector being displayed
(def dir (atom 1))  ;direction of next position (-1 or 1)

(def label (JLabel. text))

(.addMouseListener label
  (proxy [MouseAdapter] []
    (mouseClicked [evt] (swap! dir -))))

(defn animator []
  (while true
    (Thread/sleep 100)
    (swap! pos #(-> % (+ @dir) (mod text-ct)))
    (.setText label (rotations @pos))))

(doto (JFrame.)
  (.add label)
  (.pack)
  (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
  (.setVisible true))

(future-call animator)  ;simple way to run animator on a separate thread
