; linear congruential generator given in C99 section 7.20.2.1
(define ((c-rand seed)) (set! seed (remainder (+ (* 1103515245 seed) 12345) 2147483648)) (quotient seed 65536))

; uniform real numbers in open interval (0, 1)
(define (unif-rand seed) (let ((r (c-rand seed))) (lambda () (/ (+ (r) 1) 32769.0))))

; Box-Muller method to generate normal distribution
(define (normal-rand unif m s)
(let ((? #t) (! 0.0) (twopi (* 2.0 (acos -1.0))))
(lambda ()
   (set! ? (not ?))
   (if ? !
         (let ((a (sqrt (* -2.0 (log (unif))))) (b (* twopi (unif))))
              (set! ! (+ m (* s a (sin b))))
              (+ m (* s a (cos b))))))))

(define rnorm (normal-rand (unif-rand 0) 1.0 0.5))

; auxiliary function to get a list of 'n random numbers from generator 'r
(define (rand-list r n) = (if (zero? n) '() (cons (r) (rand-list r (- n 1)))))

(define v (rand-list rnorm 1000))

v
#|
(-0.27965824722565835
 -0.8870860825789542
 0.6499618744638194
 0.31336141955110863
 ...
 0.5648743998193049
 0.8282656735558756
 0.6399951934564637
 0.7699535302478072)
|#

; check mean and standard deviation
(define (mean-sdev v)
(let loop ((v v) (a 0) (b 0) (n 0))
(if (null? v)
    (let ((mean (/ a n)))
         (list mean (sqrt (/ (- b (* n mean mean)) (- n 1)))))
    (let ((x (car v)))
         (loop (cdr v) (+ a x) (+ b (* x x)) (+ n 1))))))

(mean-sdev v)
; (0.9562156817697293 0.5097087109575911)
