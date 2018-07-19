#lang racket

(define (adder-and a b)
  (if (= 2 (+ a b)) 1 0))    ; Defining the basic and function

(define (adder-not a)
  (if (zero? a) 1 0))        ; Defining the basic not function

(define (adder-or a b)
  (if (> (+ a b) 0) 1 0))    ; Defining the basic or function

(define (adder-xor a b)
  (adder-or
   (adder-and
    (adder-not a)
    b)
   (adder-and
    a
    (adder-not b))))         ; Defines the xor function based on the basic functions

(define (half-adder a b)
  (list (adder-xor a b) (adder-and a b))) ; Creates the half adder, returning '(sum carry)

(define (adder a b c0)
  (define half-a (half-adder c0 a))
  (define half-b (half-adder (car half-a) b))
  (list
   (car half-b)
   (adder-or (cadr half-a) (cadr half-b))))  ; Creates the full adder, returns '(sum carry)

(define (n-bit-adder 4a 4b)   ; Creates the n-bit adder, it receives 2 lists of same length
  (let-values                 ; Lists of the form '([01]+)
      (((4s v)                ; for/fold form will return 2 values, receiving this here
        (for/fold ((S null) (c 0)) ;initializes the full sum and carry
          ((a (in-list (reverse 4a))) (b (in-list (reverse 4b))))
          ;here it prepares variables for summing each element, starting from the least important bits
          (define added
            (adder a b c))
          (values
           (cons (car added) S) ; changes S and c to it's new values in the next iteration
           (cadr added)))))
    (if (zero? v)
        4s
        (cons v 4s))))

(n-bit-adder '(1 0 1 0) '(0 1 1 1)) ;-> '(1 0 0 0 1)
