(def dice5 #(rand-int 5))

(defn dice7 []
  (quot (->> dice5                     ; do the following to dice5
             (repeatedly 2)            ; call it twice
             (apply #(+ %1 (* 5 %2)))  ; d1 + 5*d2 => 0..24
             #()                       ; wrap that up in a function
             repeatedly                ; make infinite sequence of the above
             (drop-while #(> % 20))    ; throw away anything > 20
             first)                    ; grab first acceptable element
        3))                            ; divide by three rounding down

(doseq [n [100 1000 10000] [num count okay?] (verify dice7 n)]
  (println "Saw" num count "times:"
           (if okay? "that's" "   not") "acceptable"))
