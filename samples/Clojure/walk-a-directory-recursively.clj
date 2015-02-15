(use '[clojure.java.io])

(defn walk [dirpath pattern]
  (doall (filter #(re-matches pattern (.getName %))
                 (file-seq (file dirpath)))))

(map #(println (.getPath %)) (walk "src" #".*\.clj"))
