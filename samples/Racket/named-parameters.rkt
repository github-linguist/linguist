#lang racket

(define (pizza sauce
               ;; mandatory keyword argument
               #:topping topping
               ;; optional keyword argument with default
               #:type [type "deep dish"])
  (printf "~a pizza with ~a sauce topped with ~a~n"
          type sauce topping))

(pizza "tomato" #:topping "onion")
(pizza #:topping "onion" "garlic" #:type "pan")
