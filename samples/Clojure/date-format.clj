(let [now (.getTime (java.util.Calendar/getInstance))
      f1  (java.text.SimpleDateFormat. "yyyy-MM-dd")
      f2  (java.text.SimpleDateFormat. "EEEE, MMMM dd, yyyy")]
  (println (.format f1 now))
  (println (.format f2 now)))
