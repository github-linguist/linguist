#lang racket
;;; returns a probabalistic choice from the sequence choices
;;; choices generates two values -- the chosen value and a
;;; probability (weight) of the choice.
;;;
;;; Note that a hash where keys are choices and values are probabilities
;;; is such a sequence.
;;;
;;; if the total probability < 1 then choice could return #f
;;; if the total probability > 1 then some choices may be impossible
(define (probabalistic-choice choices)
  (let-values
      (((_ choice) ;; the fold provides two values, we only need the second
                   ;; the first will always be a negative number showing that
                   ;; I've run out of random steam
        (for/fold
            ((rnd (random))
             (choice #f))
          (((v p) choices)
           #:break (<= rnd 0))
          (values (- rnd p) v))))
    choice))

;;; ditto, but all probabilities must be exact rationals
;;; the optional lcd
;;;
;;; not the most efficient, since it provides a wrapper (and demo)
;;; for p-c/i-w below
(define (probabalistic-choice/exact
         choices
         #:gcd (GCD (/ (apply gcd (hash-values choices)))))
  (probabalistic-choice/integer-weights
   (for/hash (((k v) choices))
     (values k (* v GCD)))
   #:sum-of-weights GCD))

;;; this proves useful in Rock-Paper-Scissors
(define (probabalistic-choice/integer-weights
         choices
         #:sum-of-weights (sum-of-weights (apply + (hash-values choices))))
  (let-values
      (((_ choice)
        (for/fold
            ((rnd (random sum-of-weights))
             (choice #f))
          (((v p) choices)
           #:break (< rnd 0))
          (values (- rnd p) v))))
    choice))

(module+ test
  (define test-samples (make-parameter 1000000))

  (define (test-p-c-function f w)
    (define test-selection (make-hash))
    (for* ((i (in-range 0 (test-samples)))
           (c (in-value (f w))))
      (when (zero? (modulo i 100000)) (eprintf "~a," (quotient i 100000)))
      (hash-update! test-selection c add1 0))
    (printf "~a~%choice\tcount\texpected\tratio\terror~%" f)
    (for* (((k v) (in-hash test-selection))
           (e (in-value (* (test-samples) (hash-ref w k)))))
      (printf "~a\t~a\t~a\t~a\t~a%~%"
              k v e
              (/ v (test-samples))
              (real->decimal-string
               (exact->inexact (* 100 (/ (- v e) e)))))))

  (define test-weightings/rosetta
    (hash
     'aleph 1/5
     'beth 1/6
     'gimel 1/7
     'daleth 1/8
     'he 1/9
     'waw 1/10
     'zayin 1/11
     'heth 1759/27720; adjusted so that probabilities add to 1
     ))

  (define test-weightings/50:50 (hash 'woo 1/2 'yay 1/2))
  (define test-weightings/1:2:3 (hash 'woo 1 'yay 2 'foo 3))

  (test-p-c-function probabalistic-choice test-weightings/50:50)
  (test-p-c-function probabalistic-choice/exact test-weightings/50:50)
  (test-p-c-function probabalistic-choice test-weightings/rosetta)
  (test-p-c-function probabalistic-choice/exact test-weightings/rosetta))
