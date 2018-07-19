#lang racket

(define animal-interface (interface () say))

(define cat% (class* object% (animal-interface) (super-new))) ;; error

(define cat% (class* object% (animal-interface)
               (super-new)
               (define/public (say)
                 (display "meeeeew!"))))

(define tom (new cat%))
(send tom say)
