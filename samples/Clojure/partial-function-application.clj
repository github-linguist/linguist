(defn fs [f s] (map f s))
(defn f1 [x] (* 2 x))
(defn f2 [x] (* x x))
(def fsf1 (partial fs f1))
(def fsf2 (partial fs f2))

(doseq [s [(range 4) (range 2 9 2)]]
  (println "seq: " s)
  (println "  fsf1: " (fsf1 s))
  (println "  fsf2: " (fsf2 s)))
