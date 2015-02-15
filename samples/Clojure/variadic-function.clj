(defn foo [& args]
  (doseq [a args]
    (println a)))

(foo :bar :baz :quux)
(apply foo [:bar :baz :quux])
