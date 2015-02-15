#lang racket

(define (best-shuffle s)
  (define len (string-length s))
  (define @ string-ref)
  (define r (list->string (shuffle (string->list s))))
  (for* ([i (in-range len)] [j (in-range len)])
    (when (not (or (= i j) (eq? (@ s i) (@ r j)) (eq? (@ s j) (@ r i))))
      (define t (@ r i))
      (string-set! r i (@ r j))
      (string-set! r j t)))
  r)

(define (count-same s1 s2)
  (for/sum ([c1 (in-string s1)] [c2 (in-string s2)])
    (if (eq? c1 c2) 1 0)))

(for ([s (in-list '("tree" "abracadabra" "seesaw" "elk" "grrrrrr" "up" "a"))])
  (define sh (best-shuffle s))
  (printf " ~a, ~a, (~a)\n" s sh (count-same s sh)))
