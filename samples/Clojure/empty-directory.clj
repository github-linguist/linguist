(require '[clojure.java.io :as io])
(defn empty-dir? [path]
  (let [file (io/file path)]
    (assert (.exists file))
    (assert (.isDirectory file))
    (-> file .list empty?))) ; .list ignores "." and ".."
