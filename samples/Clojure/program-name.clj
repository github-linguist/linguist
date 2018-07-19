":";exec lein exec $0 ${1+"$@"}
":";exit

(ns scriptname
  (:gen-class))

(defn -main [& args]
  (let [program (first *command-line-args*)]
    (println "Program:" program)))

(when (.contains (first *command-line-args*) *source-path*)
  (apply -main (rest *command-line-args*)))
