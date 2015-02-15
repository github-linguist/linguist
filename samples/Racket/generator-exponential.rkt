#lang racket

(require racket/generator)

;; this is a function that returns a powers generator, not a generator
(define (powers m)
  (generator ()
    (for ([n (in-naturals)]) (yield (expt n m)))))

(define squares (powers 2))
(define cubes   (powers 3))

;; same here
(define (filtered g1 g2)
  (generator ()
    (let loop ([n1 (g1)] [n2 (g2)])
      (cond [(< n1 n2) (yield n1) (loop (g1) n2)]
            [(> n1 n2) (loop n1 (g2))]
            [else (loop (g1) (g2))]))))

(for/list ([x (in-producer (filtered squares cubes) (lambda (_) #f))]
           [i 30] #:when (>= i 20))
  x)
