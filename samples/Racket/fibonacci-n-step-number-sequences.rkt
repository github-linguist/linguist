#lang racket
;; fib-n : Nat x Nat -> [List Nat]
;; Outputs the first x numbers in the
;; n-step fibonacci sequence
;; n > 1
(define (fib-n n x)
  (cond
        [(= x 0) empty]
        [(= x 1) '(1)]
        [(= x 2) '(1 1)]
        [(<= x (add1 n)) (append '(1 1) (build-list (- x 2) (λ (y) (expt 2 (add1 y)))))]
        [else (local ((define first-values (append '(1 1) (build-list (- n 1) (λ (x) (expt 2 (add1 x))))))
                      (define (add-values lon y acc)
                        (cond [(= y 0) acc]
                              [else (add-values (rest lon) (sub1 y) (+ (first lon) acc))]))
                      (define (acc lon y)
                        (cond [(= y x) lon]
                              [else (acc (cons (add-values lon n 0) lon) (add1 y))])))
                (reverse (acc (reverse first-values) (add1 n))))]))
;; fib-list : [List Nat] x Nat -> [List Nat]
;; Given a list of natural numbers,
;; the length of the list becomes the
;; size of the step, and outputs
;; the first x numbers of the sequence
;; (len lon) > 1
(define (fib-list lon x)
  (local ((define step (length lon)))
    (cond
      [(= x step) lon]
      [(< x step)
       (local ((define (extract-values lon y)
                 (cond [(= y 0) empty]
                       [else (cons (first lon) (extract-values (rest lon) (sub1 y)))])))
         (extract-values lon x))]
      [else (local ((define (add-values lon y acc)
                      (cond [(= y 0) acc]
                            [else (add-values (rest lon) (sub1 y) (+ (first lon) acc))]))
                    (define (acc lon y)
                      (cond [(= y x) lon]
                            [else (acc (cons (add-values lon step 0) lon) (add1 y))])))
              (reverse (acc (reverse lon)  step)))])))

; Now compute the series:
(for/list ([n (in-range 2 11)])
  (fib-list (fib-n n n) 20))
