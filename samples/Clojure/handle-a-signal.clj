(require 'clojure.repl)

(def start (System/nanoTime))

(defn shutdown [_]
  (println "Received INT after"
           (/ (- (System/nanoTime) start) 1e9)
           "seconds.")
  (System/exit 0))

(clojure.repl/set-break-handler! shutdown)

(doseq [i (range)]
  (prn i)
  (Thread/sleep 500))
