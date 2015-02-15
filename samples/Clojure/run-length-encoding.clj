(defn compress [s]
  (->> (partition-by identity s) (mapcat (juxt count first)) (apply str)))

(defn extract [s]
  (->> (re-seq #"(\d+)([A-Z])" s)
       (mapcat (fn [[_ n ch]] (repeat (Integer/parseInt n) ch)))
       (apply str)))
