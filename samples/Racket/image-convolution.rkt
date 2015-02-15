#lang typed/racket
(require images/flomap racket/flonum)

(provide flomap-convolve)

(: perfect-square? (Nonnegative-Fixnum -> (U Nonnegative-Fixnum #f)))
(define (perfect-square? n)
  (define rt-n (integer-sqrt n))
  (and (= n (sqr rt-n)) rt-n))

(: flomap-convolve (flomap FlVector -> flomap))
(define (flomap-convolve F K)
  (unless (flomap? F) (error "arg1 not a flowmap"))
  (unless (flvector? K) (error "arg2 not a flvector"))
  (define R (perfect-square? (flvector-length K)))
  (cond
    [(not (and R (odd? R))) (error "K is not odd-sided square")]
    [else
     (define R/2 (quotient R 2))
     (define R/-2 (quotient R -2))
     (define-values (sz-w sz-h) (flomap-size F))
     (define-syntax-rule (convolution c x y i)
       (if (= 0 c)
           (flomap-ref F c x y) ; c=3 is alpha channel
           (for*/fold: : Flonum
             ((acc : Flonum 0.))
             ((k (in-range 0 (add1 R/2)))
              (l (in-range 0 (add1 R/2)))
              (kl (in-value (+ (* k R) l)))
              (kx (in-value (+ x k R/-2)))
              (ly (in-value (+ y l R/-2)))
              #:when (< 0 kx (sub1 sz-w))
              #:when (< 0 ly (sub1 sz-h)))
             (+ acc (* (flvector-ref K kl) (flomap-ref F c kx ly))))))

     (inline-build-flomap 4 sz-w sz-h convolution)]))

(module* test racket
  (require racket/draw images/flomap racket/flonum (only-in 2htdp/image save-image))
  (require (submod ".."))
  (define flmp (bitmap->flomap (read-bitmap "jpg/271px-John_Constable_002.jpg")))
  (save-image
   (flomap->bitmap (flomap-convolve flmp (flvector 1.)))
   "out/convolve-unit-1x1.png")
  (save-image
   (flomap->bitmap (flomap-convolve flmp (flvector 0. 0. 0. 0. 1. 0. 0. 0. 0.)))
   "out/convolve-unit-3x3.png")
  (save-image
   (flomap->bitmap (flomap-convolve flmp (flvector -1. -1. -1. -1. 4. -1. -1. -1. -1.)))
   "out/convolve-etch-3x3.png"))
