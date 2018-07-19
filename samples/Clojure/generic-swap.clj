(defn swap [pair] (reverse pair))    ; returns a list
(defn swap [[a b]] '(b a))           ; returns a list
(defn swap [[a b]] [b a])            ; returns a vector
