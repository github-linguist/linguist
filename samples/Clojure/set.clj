(require 'clojure.set)

; sets can be created using the set method or set literal syntax
(def a (set [1 2 3 4]))
(def b #{4 5 6 7})

(a 10) ; returns the element if it's contained in the set, otherwise nil

(clojure.set/union a b)

(clojure.set/intersection a b)

(clojure.set/difference a b)

(clojure.set/subset? a b)
