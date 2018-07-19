(defn sort [table & {:keys [ordering column reverse?]
                     :or {ordering :lex, column 1}}]
  (println table ordering column reverse?))

(sort [1 8 3] :reverse? true)
[1 8 3] :lex 1 true
