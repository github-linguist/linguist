#lang racket

(define (mean-angle αs)
  (radians->degrees
   (mean-angle/radians
    (map degrees->radians αs))))

(define (mean-angle/radians αs)
  (define n (length αs))
  (atan (* (/ 1 n) (for/sum ([α_j αs]) (sin α_j)))
        (* (/ 1 n) (for/sum ([α_j αs]) (cos α_j)))))

(mean-angle '(350 0 10))
(mean-angle '(90 180 270 360))
(mean-angle '(10 20 30))
