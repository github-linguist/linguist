#lang racket

(define (get-last-door a b) ; assumes a != b
  (vector-ref '#(- 2 1
                 2 - 0
                 1 0 -)
              (+ a (* 3 b))))

(define (run-game strategy)
  (define car-door (random 3))
  (define first-choice (random 3))
  (define revealed-goat
    (if (= car-door first-choice)
      (let ([r (random 2)]) (if (<= car-door r) (add1 r) r)) ; random
      (get-last-door car-door first-choice))) ; reveal goat
  (define final-choice (strategy first-choice revealed-goat))
  (define win? (eq? final-choice car-door))
  ;; (printf "car: ~s\nfirst: ~s\nreveal: ~s\nfinal: ~s\n  => ~s\n\n"
  ;;         car-door first-choice revealed-goat final-choice
  ;;         (if win? 'win 'lose))
  win?)

(define (keep-choice first-choice revealed-goat)
  first-choice)

(define (change-choice first-choice revealed-goat)
  (get-last-door first-choice revealed-goat))

(define (test-strategy strategy)
  (define N 10000000)
  (define wins (for/sum ([i (in-range N)]) (if (run-game strategy) 1 0)))
  (printf "~a: ~a%\n"
          (object-name strategy)
          (exact->inexact (/ wins N 1/100))))

(for-each test-strategy (list keep-choice change-choice))
