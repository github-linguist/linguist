(let
  [words    (re-seq #"\w+" (slurp "unixdict.txt"))
   anagrams (filter second (vals (group-by sort words)))
   deranged (remove #(some true? (apply map = %)) anagrams)]
  (prn (last (sort-by #(count (first %)) deranged))))
