#lang racket

(define namespaces
  (for/list ([i (in-range 1 13)])
    (define ns (make-base-namespace))
    (eval `(begin (define N ,i) (define count 0)) ns)
    ns))

(define (get-var-values name)
  (map (curry namespace-variable-value name #t #f) namespaces))

(define code
  '(when (> N 1)
     (set! N (if (even? N) (/ N 2) (+ 1 (* N 3))))
     (set! count (add1 count))))

(define (show-nums nums)
  (for ([n nums]) (display (~a n #:width 4 #:align 'right)))
  (newline))

(let loop ()
  (define Ns (get-var-values 'N))
  (show-nums Ns)
  (unless (andmap (λ(n) (= n 1)) Ns)
    (for ([ns namespaces]) (eval code ns))
    (loop)))
(displayln (make-string (* 4 12) #\=))
(show-nums (get-var-values 'count))
