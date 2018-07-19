(defprotocol Printable
  (print-it [this] "Prints out the Printable."))

(deftype Point [x y]
  Printable
  (print-it [this] (println (str "Point: " x " " y))))

(defn create-point
  "Redundant constructor function."
  [x y] (Point. x y))

(deftype Circle [x y r]
  Printable
  (print-it [this] (println (str "Circle: " x " " y " " r))))

(defn create-circle
  "Redundant consturctor function."
  [x y r] (Circle. x y r))
