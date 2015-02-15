(let [s "I am a string"]
  ;; match
  (when (re-find #"string$" s)
    (println "Ends with 'string'."))
  (when-not (re-find #"^You" s)
    (println "Does not start with 'You'."))

  ;; substitute
  (println (clojure.string/replace s " a " " another "))
)
