#lang racket

(require 2htdp/image 2htdp/universe)

(define (pendulum)
  (define (accel θ) (- (sin θ)))
  (define θ (/ pi 2.5))
  (define θ′ 0)
  (define θ′′ (accel (/ pi 2.5)))
  (define (x θ) (+ 200 (* 150 (sin θ))))
  (define (y θ) (* 150 (cos θ)))
  (λ (n)
    (define p-image (underlay/xy (add-line (empty-scene 400 200) 200 0 (x θ) (y θ) "black")
                                 (- (x θ) 5) (- (y θ) 5) (circle 5 "solid" "blue")))
    (set! θ (+ θ (* θ′ 0.04)))
    (set! θ′ (+ θ′ (* (accel θ) 0.04)))
    p-image))

(animate (pendulum))
