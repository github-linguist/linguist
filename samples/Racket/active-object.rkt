#lang racket

(require (only-in racket/gui sleep/yield timer%))

(define active%
  (class object%
    (super-new)
    (init-field k) ; input function
    (field [s 0])  ; state
    (define t_0 0)

    (define/public (input new-k) (set! k new-k))
    (define/public (output) s)

    (define (callback)
      (define t_1 (/ (- (current-inexact-milliseconds) start) 1000))
      (set! s (+ s (* (+ (k t_0) (k t_1))
                      (/ (- t_1 t_0) 2))))
      (set! t_0 t_1))

    (define start (current-inexact-milliseconds))
    (new timer%
         [interval 1000]
         [notify-callback callback])))

(define active (new active% [k (λ (t) (sin (* 2 pi 0.5 t)))]))
(sleep/yield 2)
(send active input (λ _ 0))
(sleep/yield 0.5)
(displayln (send active output))
