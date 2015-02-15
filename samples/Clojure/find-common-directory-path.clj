(use '[clojure.string :only [join,split]])

(defn common-prefix [sep paths]
  (let [parts-per-path (map #(split % (re-pattern sep)) paths)
        parts-per-position (apply map vector parts-per-path)]
    (join sep
      (for [parts parts-per-position :while (apply = parts)]
        (first parts)))))

(println
  (common-prefix "/"
    ["/home/user1/tmp/coverage/test"
     "/home/user1/tmp/covert/operator"
     "/home/user1/tmp/coven/members"]))
