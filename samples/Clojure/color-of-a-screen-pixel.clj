(defn get-color-at [x y]
  (.getPixelColor (java.awt.Robot.) x y))
