(defmacro ->
  [& operations]
  (reduce-list
   (rest operations)
   (fn [form operation]
     (cons (first operation)
           (cons form (rest operation))))
   (first operations)))


(->
 (open target :keypress)
 (filter enter-key?)
 (map get-input-text)
 (reduce render))
