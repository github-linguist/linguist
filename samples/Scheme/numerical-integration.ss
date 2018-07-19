(define (integrate f a b steps meth)
  (define h (/ (- b a) steps))
  (* h
     (let loop ((i 0) (s 0))
       (if (>= i steps)
           s
           (loop (+ i 1) (+ s (meth f (+ a (* h i)) h)))))))

(define (left-rect f x h) (f x))
(define (mid-rect f x h) (f (+ x (/ h 2))))
(define (right-rect f x h) (f (+ x h)))
(define (trapezium f x h) (/ (+ (f x) (f (+ x h))) 2))
(define (simpson f x h) (/ (+ (f x) (* 4 (f (+ x (/ h 2)))) (f (+ x h))) 6))

(define (square x) (* x x))

(define rl (integrate square 0 1 10 left-rect))
(define rm (integrate square 0 1 10 mid-rect))
(define rr (integrate square 0 1 10 right-rect))
(define t (integrate square 0 1 10 trapezium))
(define s (integrate square 0 1 10 simpson))
