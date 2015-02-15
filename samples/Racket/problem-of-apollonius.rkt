#lang slideshow

(struct circle (x y r) #:prefab)

(define (apollonius c1 c2 c3 s1 s2 s3)
  (define x1 (circle-x c1))
  (define y1 (circle-y c1))
  (define r1 (circle-r c1))
  (define x2 (circle-x c2))
  (define y2 (circle-y c2))
  (define r2 (circle-r c2))
  (define x3 (circle-x c3))
  (define y3 (circle-y c3))
  (define r3 (circle-r c3))

  (define v11 (- (* 2 x2) (* 2 x1)))
  (define v12 (- (* 2 y2) (* 2 y1)))
  (define v13 (+ (- (* x1 x1) (* x2 x2))
                 (- (* y1 y1) (* y2 y2))
                 (- (* r2 r2) (* r1 r1))))
  (define v14 (- (* 2 s2 r2) (* 2 s1 r1)))

  (define v21 (- (* 2 x3) (* 2 x2)))
  (define v22 (- (* 2 y3) (* 2 y2)))
  (define v23 (+ (- (* x2 x2) (* x3 x3))
                 (- (* y2 y2) (* y3 y3))
                 (- (* r3 r3) (* r2 r2))))
  (define v24 (- (* 2 s3 r3) (* 2 s2 r2)))

  (define w12 (/ v12 v11))
  (define w13 (/ v13 v11))
  (define w14 (/ v14 v11))

  (define w22 (- (/ v22 v21) w12))
  (define w23 (- (/ v23 v21) w13))
  (define w24 (- (/ v24 v21) w14))

  (define P (- (/ w23 w22)))
  (define Q (/ w24 w22))
  (define M (- (+ (* w12 P) w13)))
  (define N (- w14 (* w12 Q)))

  (define a (+ (* N N) (* Q Q) -1))
  (define b (+ (- (* 2 M N) (* 2 N x1))
               (- (* 2 P Q) (* 2 Q y1))
               (* 2 s1 r1)))
  (define c (- (+ (* x1 x1) (* M M) (* P P) (* y1 y1))
               (+ (* 2 M x1) (* 2 P y1) (* r1 r1))))

  (define D (- (* b b) (* 4 a c)))
  (define rs (/ (- (+ b (sqrt D))) (* 2 a)))
  (define xs (+ M (* N rs)))
  (define ys (+ P (* Q rs)))
  (circle xs ys rs))

(define c1 (circle 0.0 0.0 1.0))
(define c2 (circle 4.0 0.0 1.0))
(define c3 (circle 2.0 4.0 2.0))

;; print solutions
(apollonius c1 c2 c3 1.0 1.0 1.0)
(apollonius c1 c2 c3 -1.0 -1.0 -1.0)

;; visualize solutions
(require racket/gui/base)
(define (show-circles . circles+colors)
  (define f (new frame% [label "Apollonius"] [width 300] [height 300]))
  (define c
    (new canvas% [parent f]
      [paint-callback
       (lambda (canvas dc)
         (send* dc (set-origin 100 100)
                   (set-scale 20 20)
                   (set-pen "black" 1/10 'solid)
                   (set-brush "white" 'transparent))
         (for ([x circles+colors])
           (if (string? x)
             (send dc set-pen x 1/5 'solid)
             (let ([x (circle-x x)] [y (circle-y x)] [r (circle-r x)])
               (send dc draw-ellipse (- x r) (- y r) (* 2 r) (* 2 r))))))]))
  (send f show #t))
(show-circles "black" c1 c2 c3
              "green" (apollonius c1 c2 c3 1.0 1.0 1.0)
              "red"   (apollonius c1 c2 c3 -1.0 -1.0 -1.0))
