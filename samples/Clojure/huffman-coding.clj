(use 'clojure.contrib.seq-utils)

(defn probs [items]
  (let [freqs (frequencies items) sum (reduce + (vals freqs))]
    (into {} (map (fn [[k v]] [k (/ v sum)]) freqs))))

(defn init-pq [weighted-items]
  (let [comp (proxy [java.util.Comparator] []
                (compare [a b] (compare (:priority a) (:priority b))))
        pq (java.util.PriorityQueue. (count weighted-items) comp)]
    (doseq [[item prob] weighted-items] (.add pq { :symbol item, :priority prob }))
    pq))

(defn huffman-tree [pq]
  (while (> (.size pq) 1)
    (let [a (.poll pq) b (.poll pq) new-node { :priority (+ (:priority a) (:priority b)), :left a, :right b }]
      (.add pq new-node)))
  (.poll pq))

(defn symbol-map
  ([t] (into {} (symbol-map t [])))
  ([{:keys [symbol,left,right] :as t} code]
    (if symbol [[symbol code]]
      (concat (symbol-map left (conj code 0))
              (symbol-map right (conj code 1))))))

(defn huffman-encode [items]
  (-> items probs init-pq huffman-tree symbol-map))

(defn display-huffman-encode [s]
  (println "SYMBOL\tWEIGHT\tHUFFMAN CODE")
  (let [probs (probs (seq s))]
    (doseq [[char code] (huffman-encode (seq s))]
      (printf "%s:\t\t%s\t\t%s\n" char (probs char) (apply str code)))))

(display-huffman-encode "this is an example for huffman encoding")
