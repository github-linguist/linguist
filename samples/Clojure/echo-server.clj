(use '[clojure.contrib.server-socket :only (create-server)])
(use '[clojure.contrib.duck-streams :only (read-lines write-lines)])

(defn echo [input output]
  (write-lines (java.io.PrintWriter. output true) (read-lines input)))

(create-server 12321 echo)
