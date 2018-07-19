;clojure is a language built with immutable/persistent data structures. there is no concept of changing what a vector/list
;is, instead clojure creates a new array with an added value using (conj...)
;in the example below the my-list does not change.


user=> (def my-list (list 1 2 3 4 5))

user=> my-list
(1 2 3 4 5)

user=> (first my-list)
1

user=> (nth my-list 3)
4

user=> (conj my-list 100) ;adding to a list always adds to the head of the list
(100 1 2 3 4 5)

user=> my-list ;it is impossible to change the list pointed to by my-list
(1 2 3 4 5)

user=> (def my-new-list (conj my-list 100))

user=> my-new-list
(100 1 2 3 4 5)

user=> (cons 200 my-new-list) ;(cons makes a new list, (conj will make a new object of the same type as the one it is given
(200 100 1 2 3 4 5)

user=> (def my-vec [1 2 3 4 5 6])

user=> (conj my-vec 300) ;adding to a vector always adds to the end of the vector
[1 2 3 4 5 6 300]
