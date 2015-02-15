(defn basic-input [fname]
  (line-seq (java.io.BufferedReader. (java.io.FileReader. fname))))
