#lang racket

(define (update cells)
  (for/list ([crowding (map +
                            (append '(0) (drop-right cells 1))
                            cells
                            (append (drop cells 1) '(0)))])
    (if (= 2 crowding) 1 0)))

(define (life-of cells time)
  (unless (zero? time)
    (displayln cells)
    (life-of (update cells) (sub1 time))))

(life-of '(0 1 1 1 0 1 1 0 1 0 1 0 1 0 1 0 0 1 0 0)
         10)

#| (0 1 1 1 0 1 1 0 1 0 1 0 1 0 1 0 0 1 0 0)
   (0 1 0 1 1 1 1 1 0 1 0 1 0 1 0 0 0 0 0 0)
   (0 0 1 1 0 0 0 1 1 0 1 0 1 0 0 0 0 0 0 0)
   (0 0 1 1 0 0 0 1 1 1 0 1 0 0 0 0 0 0 0 0)
   (0 0 1 1 0 0 0 1 0 1 1 0 0 0 0 0 0 0 0 0)
   (0 0 1 1 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0)
   (0 0 1 1 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0)
   (0 0 1 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
   (0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
   (0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) |#
