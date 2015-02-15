#lang racket
(require math)

(define history (make-hash '((paper . 1) (scissors . 1) (rock . 1))))
(define total 3)

(define (update-history! human-choice)
  (set! total (+ total 1))
  (hash-update! history human-choice add1 0))

(define (pick-one)
  (sample
   (discrete-dist '(paper scissors rock)
                  (map (λ (x) (hash-ref history x))
                       '(scissors paper rock)))))

(define (find-winner computer human)
  (define order '(scissors paper rock scissors))
  (cond
    [(eq? computer human)                         'none]
    [(eq? (second (member computer order)) human) 'computer]
    [                                             'human]))

(define (game-loop)
  (define computer-choice (pick-one))
  (define human-choice (read))
  (define winner (find-winner computer-choice human-choice))
  (update-history! human-choice)
  (displayln (~a "Computer picked " computer-choice ", "
                 "human picked " human-choice ", "
                 winner " wins."))
  (game-loop))

(game-loop)
