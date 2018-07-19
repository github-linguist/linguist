(defn make-queue []
  (atom []))

(defn enqueue [q x]
  (swap! q conj x))

(defn dequeue [q]
  (if (seq @q)
    (let [x (first @q)]
      (swap! q subvec 1)
      x)
    (throw (IllegalStateException. "Can't pop an empty queue."))))

(defn queue-empty? [q]
  (empty? @q))
