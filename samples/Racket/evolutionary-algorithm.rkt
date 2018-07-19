#lang racket

(define alphabet " ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define (randch) (string-ref alphabet (random 27)))

(define (fitness s1 s2)
  (for/sum ([c1 (in-string s1)] [c2 (in-string s2)])
    (if (eq? c1 c2) 1 0)))

(define (mutate s P)
  (define r (string-copy s))
  (for ([i (in-range (string-length r))] #:when (<= (random) P))
    (string-set! r i (randch)))
  r)

(define (evolution target C P)
  (let loop ([parent (mutate target 1.0)] [n 0])
    ;; (printf "~a: ~a\n" n parent)
    (if (equal? parent target)
      n
      (let cloop ([children (for/list ([i (in-range C)]) (mutate parent P))]
                  [best #f] [fit -1])
        (if (null? children)
          (loop best (add1 n))
          (let ([f (fitness target (car children))])
            (if (> f fit)
              (cloop (cdr children) (car children) f)
              (cloop (cdr children) best fit))))))))

;; Some random experiment using all of this
(define (try-run C P)
  (define ns
    (for/list ([i 10])
      (evolution "METHINKS IT IS LIKE A WEASEL" C P)))
  (printf "~s Average generation: ~s\n" C (/ (apply + 0.0 ns) (length ns)))
  (printf "~s      Total strings: ~s\n" C (for/sum ([n ns]) (* n 50))))
(for ([C (in-range 10 501 10)]) (try-run C 0.001))
