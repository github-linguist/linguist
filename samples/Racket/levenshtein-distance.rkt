#lang racket

(define (levenshtein a b)
  (define (ls0 a-index b-index)
    (cond [(or (= a-index -1) (= b-index -1)) (abs (- a-index b-index))]
          [else
           (define a-char (string-ref a a-index))
           (define b-char (string-ref b b-index))
           (if (equal? a-char b-char)
               (ls (sub1 a-index) (sub1 b-index))
               (min (add1 (ls (sub1 a-index) b-index))
                    (add1 (ls a-index (sub1 b-index)))
                    (add1 (ls (sub1 a-index) (sub1 b-index)))))]))
  (define memo (make-hash))
  (define (ls a-i b-i)
    (hash-ref! memo (cons a-i b-i) (λ() (ls0 a-i b-i))))
  (ls (sub1 (string-length a)) (sub1 (string-length b))))

(levenshtein "kitten" "sitting")
(levenshtein "rosettacode" "raisethysword")
