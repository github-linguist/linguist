(use '[clojure.contrib.seq-utils :only (group-by)])

(defstruct employee :Name :ID :Salary :Department)

(def data
     (->> '(("Tyler Bennett" E10297 32000 D101)
            ("John Rappl" E21437 47000 D050)
            ("George Woltman" E00127 53500 D101)
            ("Adam Smith" E63535 18000 D202)
            ("Claire Buckman" E39876 27800 D202)
            ("David McClellan" E04242 41500 D101)
            ("Rich Holcomb" E01234 49500 D202)
            ("Nathan Adams" E41298 21900 D050)
            ("Richard Potter" E43128 15900 D101)
            ("David Motsinger" E27002 19250 D202)
            ("Tim Sampair" E03033 27000 D101)
            ("Kim Arlich" E10001 57000 D190)
            ("Timothy Grove" E16398 29900 D190))
          (map #(apply (partial struct employee) %))))


(doseq [[dep emps] (group-by :Department data)]
  (println "Department:" dep)
  (doseq [e (take 3 (reverse (sort-by :Salary emps)))]
    (println e)))
