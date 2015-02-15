(import '[java.awt Color Graphics Image]
	'[java.awt.image BufferedImage])

(defn blank-bitmap [width height]
  (BufferedImage. width height BufferedImage/TYPE_3BYTE_BGR))

(defn fill [image color]
  (doto (.getGraphics image)
    (.setColor color)
    (.fillRect 0 0 (.getWidth image) (.getHeight image))))

(defn set-pixel [image x y color]
  (.setRGB image x y (.getRGB color)))

(defn get-pixel [image x y]
  (Color. (.getRGB image x y)))
