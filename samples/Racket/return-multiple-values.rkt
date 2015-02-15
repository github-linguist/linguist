#lang racket
(values 4 5)

(define (my-values . return-list)
  (call/cc
   (lambda (return)
     (apply return return-list))))
