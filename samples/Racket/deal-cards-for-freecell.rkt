#lang racket

(module Linear_congruential_generator racket
  ;; taken from http://rosettacode.org/wiki/Linear_congruential_generator#Racket
  ;; w/o BSD generator
  (require racket/generator)
  (provide ms-rand)
  (define (ms-update state_n)
    (modulo (+ (* 214013 state_n) 2531011)
            (expt 2 31)))
  (define ((rand update ->rand) seed)
    (generator () (let loop ([state_n seed])
                    (define state_n+1 (update state_n))
                    (yield (->rand state_n+1))
                    (loop state_n+1))))
  (define ms-rand (rand ms-update (lambda (x) (quotient x (expt 2 16))))))

(require (submod "." Linear_congruential_generator))

;; Personally I prefer CDHS to the unicode characters (on an aesthetic basis,
;; rather than anything else. Plus it helps match with the examples given at the
;; head of the task.
(define suits "CDHS")
(define (initial-deck)
  (for*/vector #:length 52
    ((face "A23456789TJQK")
     (suit suits))
    (cons face suit)))

;; srfi/43 has one of these, but is quick enough to reimplement!
(define (vector-swap! v i j)
  (let ((t (vector-ref v i)))
    (vector-set! v i (vector-ref v j))
    (vector-set! v j t)))

(define (deal hand)
  (define pack (initial-deck))
  (define rnd (ms-rand hand))
  (define (deal-nth-card pack-sz card-no deal)
    (vector-swap! pack card-no (sub1 pack-sz))
    (cons (vector-ref pack (sub1 pack-sz)) deal))

  (let inner-deal ((pack-sz (vector-length pack)) (deal null))
    (if (zero? pack-sz) (reverse deal) ;; we accumulated this backwards!
        (inner-deal (sub1 pack-sz)
                    (deal-nth-card pack-sz (modulo (rnd) pack-sz) deal)))))

(define (present-deal hand)
  (printf "Game #~a~%" hand)
  (let inner-present-deal ((pile 0) (deck (deal hand)))
    (unless (null? deck)
      (printf "~a~a~a" (caar deck) (cdar deck)
              (if (or (null? (cdr deck)) (= 7 (modulo pile 8))) "\n" " "))
      (inner-present-deal (add1 pile) (cdr deck)))))

;; Run it so we get some output:
(present-deal 1)
(newline)
(present-deal 617)
