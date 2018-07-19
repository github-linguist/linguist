#lang racket

(define almost-all
  '([A B C D] [C A B D] [A C D B] [D A C B] [B C D A] [A C B D] [A D C B]
    [C D A B] [D A B C] [B C A D] [C A D B] [C D B A] [C B A D] [A B D C]
    [A D B C] [B D C A] [D C B A] [B A C D] [B A D C] [B D A C] [C B D A]
    [D B C A] [D C A B]))


;; Obvious method:
(for/first ([p (in-permutations (car almost-all))]
            #:unless (member p almost-all))
  p)
;; -> '(D B A C)


;; For permutations of any set
(define charmap
  (for/hash ([x (in-list (car almost-all))] [i (in-naturals)])
    (values x i)))
(define size (hash-count charmap))

;; Illustrating approach mentioned in the task description.
;; For each position, character with odd parity at that position.

(require data/bit-vector)

(for/list ([i (in-range size)])
  (define parities (make-bit-vector size #f))
  (for ([permutation (in-list almost-all)])
    (define n (hash-ref charmap (list-ref permutation i)))
    (bit-vector-set! parities n (not (bit-vector-ref parities n))))
  (for/first ([(c i) charmap] #:when (bit-vector-ref parities i))
    c))
;; -> '(D B A C)
