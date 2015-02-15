(let [point (.. java.awt.MouseInfo getPointerInfo getLocation)] [(.getX point) (.getY point)])
