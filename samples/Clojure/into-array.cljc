(defn into-array
  ([aseq]
     (into-array nil aseq))
  ([type aseq]
     (let [n (count aseq)
           a (make-array n)]
       (loop [aseq (seq aseq)
              i 0]
         (if (< i n)
           (do
             (aset a i (first aseq))
             (recur (next aseq) (inc i)))
           a)))))
