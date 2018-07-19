#lang racket

;; To see these calls we do two things: mutate the binding to prevent
;; Racket from inlining the value; use a (void) call at the end so the
;; calls are not tail calls (which will otherwise not show on the
;; stack).
(define foo #f)
(set! foo (λ() (bar) (void)))
(define bar #f)
(set! bar (λ() (show-stacktrace) (void)))

(define (show-stacktrace)
  (for ([s (continuation-mark-set->context (current-continuation-marks))]
        [i (in-naturals)])
    ;; show just the names, not the full source information
    (when (car s) (printf "~s: ~s\n" i (car s)))))
(foo)
