#lang racket

;; Using a combination of the two comparisons
(define (sort1 words)
  (sort words (λ(x y)
                (define xl (string-length x)) (define yl (string-length y))
                (or (> xl yl) (and (= xl yl) (string-ci<? x y))))))
(sort1 '("Some" "pile" "of" "words"))
;; -> '("words" "pile" "Some" "of")

;; Doing two sorts, relying on `sort's stability
(define (sort2 words)
  (sort (sort words string-ci<?) > #:key string-length))
(sort2 '("Some" "pile" "of" "words"))
;; -> '("words" "pile" "Some" "of")
