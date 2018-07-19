#lang racket

(define (pig-the-dice #:print? [print? #t] . players)
  (define prn (if print? (λ xs (apply printf xs) (flush-output)) void))
  (define names (for/list ([p players] [n (in-naturals 1)]) n))
  (define points (for/list ([p players]) (box 0)))
  (with-handlers ([(negate exn?) identity])
    (for ([nm (in-cycle names)] [tp (in-cycle points)] [pl (in-cycle players)])
      (prn (string-join (for/list ([n names] [p points])
                          (format "Player ~a, ~a points" n (unbox p)))
                        "; " #:before-first "Status: " #:after-last ".\n"))
      (let turn ([p 0] [n 0])
        (prn "Player ~a, round #~a, [R]oll or [P]ass? " nm (+ 1 n))
        (define roll? (pl (unbox tp) p n))
        (unless (eq? pl human) (prn "~a\n" (if roll? 'R 'P)))
        (if (not roll?) (set-box! tp (+ (unbox tp) p))
            (let ([r (+ 1 (random 6))])
              (prn "  Dice roll: ~s => " r)
              (if (= r 1) (prn "turn lost\n")
                  (let ([p (+ p r)]) (prn "~a points\n" p) (turn p (+ 1 n)))))))
      (prn "--------------------\n")
      (when (<= 100 (unbox tp)) (prn "Player ~a wins!\n" nm) (raise nm)))))

(define (human total-points turn-points round#)
  (case (string->symbol (car (regexp-match #px"[A-Za-z]?" (read-line))))
    [(R r) #t] [(P p) #f] [else (human total-points turn-points round#)]))

;; Always do N rolls
(define ((n-rounds n) total-points turn-points round#) (< round# n))
;; Roll until a given number of points
(define ((n-points n) total-points turn-points round#) (< turn-points n))
;; Random decision
(define ((n-random n) total-points turn-points round#) (zero? (random n)))

(define (n-runs n . players)
  (define v (make-vector (length players) 0))
  (for ([i n])
    (define p (sub1 (apply pig-the-dice #:print? #f players)))
    (vector-set! v p (add1 (vector-ref v p))))
  (for ([wins v] [i (in-naturals 1)])
    (printf "Player ~a: ~a%\n" i (round (/ wins n 1/100)))))

;; Things to try
;; (n-runs 1000 (n-random 2) (n-random 3) (n-random 4))
;; (n-runs 1000 (n-rounds 5) (n-points 24))
;; (n-runs 1000 (n-rounds 5) (n-random 2))
