(defn sleep [ms] ; time in milliseconds
  (println "Sleeping...")
  (Thread/sleep ms)
  (println "Awake!"))
; call it
(sleep 1000)
