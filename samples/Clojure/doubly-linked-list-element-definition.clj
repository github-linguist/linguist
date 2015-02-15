(defrecord Node [prev next data])

(defn new-node [prev next data]
  (Node. (ref prev) (ref next) data))
