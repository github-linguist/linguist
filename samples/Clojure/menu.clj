(defn menu [prompt choices]
  (if (empty? choices)
    ""
    (let [menutxt (apply str (interleave
                              (iterate inc 1)
                              (map #(str \space % \newline) choices)))]
      (println menutxt)
      (print prompt)
      (flush)
      (let [index (read-string (read-line))]
        ; verify
        (if (or (not (integer? index))
                (> index (count choices))
                (< index 1))
          ; try again
          (recur prompt choices)
          ; ok
          (nth choices (dec index)))))))

(println "You chose: "
         (menu "Which is from the three pigs: "
               ["fee fie" "huff and puff" "mirror mirror" "tick tock"]))
