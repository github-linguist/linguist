(defn myfunc []
  (println "Enter x and y")
  (let [x (read), y (read)]
    (doseq [op '(+ - * / Math/pow rem)]
      (let [exp (list op x y)]
	(printf "%s=%s\n" exp (eval exp))))))
