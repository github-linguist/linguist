(import '[java.io File])
(defn show-size [filename]
  (println filename "size:" (.length (File. filename))))

(show-size "input.txt")
(show-size "/input.txt")
