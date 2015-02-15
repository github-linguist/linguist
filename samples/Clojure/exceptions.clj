(try
  (if (> (rand) 0.5)
    (throw (RuntimeException. "oops!"))
  (println "see this half the time")
  (catch RuntimeException e
    (println e)
  (finally
    (println "always see this"))
