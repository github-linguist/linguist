(defn append-hello [s]
  (str "Hello " s))

(defn modify-string [f s]
  (f s))

(println (modify-string append-hello "World!"))
