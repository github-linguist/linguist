#lang racket

(define dict1 #hash((apple . 5) (orange . 10))) ; hash table
(define dict2 '((apple . 5) (orange . 10)))     ; a-list
(define dict3 (vector "a" "b" "c"))             ; vector (integer keys)

(dict-keys dict1)                   ; => '(orange apple)
(dict-values dict2)                 ; => '(5 10)
(for/list ([(k v) (in-dict dict3)]) ; => '("0 -> a" "1 -> b" "2 -> c")
  (format "~a -> ~a" k v))
