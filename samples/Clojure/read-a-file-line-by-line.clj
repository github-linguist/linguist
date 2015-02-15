(with-open [r (clojure.java.io/reader "some-file.txt")]
   (doseq [l (line-seq r)]
     (println l)))
