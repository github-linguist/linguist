; check Java version
(let [version (Double/parseDouble (re-find #"\d*\.\d*" (System/getProperty "java.version")))]
  (if (>= version 1.5)
    (println "Version ok")
    (throw (Error. "Bad version"))))

; check Clojure version
(let [version (Double/parseDouble (re-find #"\d*\.\d*" (clojure-version)))]
  (if (>= version 1.0)
    (println "Version ok")
    (throw (Error. "Bad version"))))
