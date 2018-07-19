#lang racket
(define (deg p)
  (for/fold ([d -inf.0]) ([(pi i) (in-indexed p)])
    (if (zero? pi) d i)))
(define (lead p) (vector-ref p (deg p)))
(define (mono c d) (build-vector (+ d 1) (λ(i) (if (= i d) c 0))))
(define (poly*cx^n c n p) (vector-append (make-vector n 0) (for/vector ([pi p]) (* c pi))))
(define (poly+ p q) (poly/lin 1 p  1 q))
(define (poly- p q) (poly/lin 1 p -1 q))
(define (poly/lin a p b q)
  (cond [(< (deg p) 0) q]
        [(< (deg q) 0) p]
        [(< (deg p) (deg q)) (poly/lin b q a p)]
        [else (define ap+bq (for/vector #:length (+ (deg p) 1) #:fill 0
                              ([pi p] [qi q]) (+ (* a pi) (* b qi))))
              (for ([i (in-range (+ (deg q) 1) (+ (deg p) 1))])
                (vector-set! ap+bq i (* a (vector-ref p i))))
              ap+bq]))

(define (poly/ n d)
  (define N (deg n))
  (define D (deg d))
  (cond
    [(< N 0) (error 'poly/ "can't divide by zero")]
    [(< N D) (values 0 n)]
    [else    (define c (/ (lead n) (lead d)))
             (define q (mono c (- N D)))
             (define r (poly- n (poly*cx^n c (- N D) d)))
             (define-values (q1 r1) (poly/ r d))
             (values (poly+ q q1) r1)]))
; Example:
(poly/ #(-42 0 -12 1) #(-3 1))
; Output:
'#(-27 -9 1)
'#(-123 0)
