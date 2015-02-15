(doseq [[k v] {:a 1, :b 2, :c 3}]
  (println k "=" v))

(doseq [k  (keys {:a 1, :b 2, :c 3})]
  (println k))

(doseq [v  (vals {:a 1, :b 2, :c 3})]
  (println v))
