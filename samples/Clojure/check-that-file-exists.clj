(map #(.exists (clojure.java.io/as-file %)) '("/input.txt" "/docs" "./input.txt" "./docs"))
