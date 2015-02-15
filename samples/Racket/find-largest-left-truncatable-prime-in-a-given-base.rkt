#lang racket
(require math/number-theory)

(define (prepend-digit b d i n)
  (+ (* d (expt b i)) n))

(define (extend b i ts)
  (define ts*
    (for/list ([t (in-set ts)])
           (for/set ([d (in-range 1 b)]
                     #:when (prime? (prepend-digit b d i t)))
                    (prepend-digit b d i t))))
  (apply set-union ts*))

(define (truncables b n)
  ; return set of truncables of length n in base b
  (if (= n 1)
      (for/set ([d (in-range 1 b)] #:when (prime? d)) d)
      (extend b (- n 1) (truncables b (- n 1)))))

(define (largest b)
  (let loop ([ts (truncables b 1)]
             [n 1])
    (define ts* (extend b n ts))
    (if (set-empty? ts*)
        (apply max (set->list ts))
        (loop ts* (+ n 1)))))


(for/list ([b (in-range 3 18)])
  (define l (largest b))
  ; (displayln (list b l))
  (list b l))

; Output:
'((3 23)
  (4 4091)
  (5 7817)
  (6 4836525320399)
  (7 817337)
  (8 14005650767869)
  (9 1676456897)
  (10 357686312646216567629137)
  (11 2276005673)
  (12 13092430647736190817303130065827539)
  (13 812751503)
  (14 615419590422100474355767356763)
  (15 34068645705927662447286191)
  (16 1088303707153521644968345559987)
  (17 13563641583101))
