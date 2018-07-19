#lang racket

(struct ± (x dx) #:transparent
  #:methods gen:custom-write
  [(define (write-proc a port mode) (display (±->string a) port))])

(define/match (±+ a [b 0])
  [((± x dx) (± y dy)) (± (+ x y) (norm dx dy))]
  [((± x dx) c) (± (+ x c) dx)]
  [(_ (± y dy)) (±+ b a)])

(define/match (±* a b)
  [((± x dx) (± y dy)) (± (* x y) (* x y (norm (/ dx x) (/ dy y))))]
  [((± x dx) c) (± (* x c) (abs (* c dx)))]
  [(_ (± y dy)) (±* b a)])

(define/match (±- a [b #f])
  [(a #f) (±* -1 a)]
  [(a b) (±+ a (±* -1 b))])

(define/match (±/ a b)
  [((± x dx) (± y dy)) (± (/ x y) (/ x y (norm (/ dx x) (/ dy y))))]
  [((± _ _) c) (±* a (/ 1 c))])

(define/match (±expt a c)
  [((± x dx) c) (± (expt x c) (abs (* (expt x c) (/ dx x))))])

(define/match (norm a b)
  [((± x dx) (± y dy)) (±expt (±+ (±expt a 2) (±expt b 2)) 0.5)]
  [(x y) (sqrt (+ (sqr x) (sqr y)))])

(define/match (±->string x [places 3])
  [((± x dx) p) (string-join (map (λ (s) (real->decimal-string s p))
                                  (list x dx))" ± ")])

;; Test
;;
(define x1 (± 100 1.1))
(define y1 (± 50 1.2))
(define x2 (± 200 2.2))
(define y2 (± 100 2.3))
(norm (±- x1 x2) (±- y1 y2))
