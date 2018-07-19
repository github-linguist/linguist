#lang racket

(define fish%
  (class object%
    (super-new)

    ;; an instance variable & constructor argument
    (init-field size)

    ;; a new method
    (define/public (eat)
      (displayln "gulp!"))))

;; constructing an instance
(new fish% [size 50])
