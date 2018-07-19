#lang racket
;; define a new syntax
(define-syntax-rule
  ;; this is the new syntax we want, in sexpr syntax:
  (if2 condition1isTrue condition2isTrue
       bothConditionsAreTrue
       firstConditionIsTrue
       secondConditionIsTrue
       noConditionIsTrue)
  ;; and this is the syntax that implements it:
  (if condition1isTrue
    (if condition2isTrue
      bothConditionsAreTrue
      firstConditionIsTrue)
    (if condition2isTrue
      secondConditionIsTrue
      noConditionIsTrue)))
;; ... and that's all you need -- it now works:
(define (try x y)
  (displayln (if2 (< x 10) (< y 10)
                  "Both small"
                  "First is small"
                  "Second is small"
                  "Neither is small")))
(try 1 1)   ; Both small
(try 1 10)  ; First is small
(try 10 1)  ; Second is small
(try 10 10) ; Neither is small
