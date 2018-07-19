(ns rosettacode.notes
  (:use [clojure.string :only [join]]))

(defn notes [notes]
  (if (seq notes)
    (spit
     "NOTES.txt"
     (str (java.util.Date.) "\n" "\t"
          (join " " notes) "\n")
     :append true)
    (println (slurp "NOTES.txt"))))

(notes *command-line-args*)
