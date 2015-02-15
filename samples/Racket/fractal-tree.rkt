#lang racket
(require graphics/turtles)

(define (tree n)
  (when (> n 1)
    (draw (/ n 2))
    (tprompt (split* (turn 60) (turn -60))
             (tree (/ n 2)))
    (draw (/ n 2))
    (turn 5)
    (tree (- n 1))))

(turtles #t) (move 100) (turn 90) (move -200)
(tree 35)
(save-turtle-bitmap "tree.png" 'png)
