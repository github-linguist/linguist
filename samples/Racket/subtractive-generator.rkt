#lang racket
(define (make-initial-state a-list max-i)
  (for/fold ((state a-list))
            ((i (in-range (length a-list) max-i)))
    (append state (list (- (list-ref state (- i 2)) (list-ref state (- i 1))))))) ;from the seed and 1 creates the initial state

(define (shuffle a-list)
  (for/list ((i (in-range (length a-list))))
    (list-ref a-list (modulo (* 34 (add1 i)) 55))))  ;shuffles the state

(define (advance-state state (times 1))
  (cond ((= 0 times) state)
        (else (advance-state
               (cdr (append state
                            (list (modulo (- (list-ref state 0) (list-ref state 31))
                                          (expt 10 9)))))
                             (sub1 times)))))  ;takes a state and the times it must be advanced, and returns the new state

(define (create-substractive-generator s0)
  (define s1 1)
  (define first-state (make-initial-state (list s0 s1) 55))
  (define shuffled-state (shuffle first-state))
  (define last-state (advance-state shuffled-state 165))
  (lambda ((m (expt 10 9)))
    (define new-state (advance-state last-state))
    (set! last-state new-state)
    (modulo (car (reverse last-state)) m)))                    ;the lambda is a function with an optional argument
                                                               ;that returns a new random number each time it's called
(define rand (create-substractive-generator 292929))
(build-list 3 (lambda (_) (rand)))  ;returns a list made from the 3 wanted numbers
