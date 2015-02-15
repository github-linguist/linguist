(defn get-code [c]
  (case c
    (\B \F \P \V) 1
    (\C \G \J \K
     \Q \S \X \Z) 2
    (\D \T) 3
    \L 4
    (\M \N) 5
    \R 6
    nil)) ;(\A \E \I \O \U \H \W \Y)

(defn soundex [s]
  (let [[f & s] (.toUpperCase s)]
    (-> (map get-code s)
	distinct
	(concat , "0000")
	(->> (cons f ,)
	     (remove nil? ,)
	     (take 4 ,)
	     (apply str ,)))))
