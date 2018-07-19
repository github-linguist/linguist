#lang racket

(require rackunit)

(struct hvar (current history) #:mutable)

(define (make-hvar v) (hvar v empty))

(define (hvar-set! hv new)
  (match-define (hvar cur hist) hv)
  (set-hvar-history! hv (cons cur hist))
  (set-hvar-current! hv new))

(define (hvar-undo! hv)
  (match-define (hvar cur (cons old hist)) hv)
  (set-hvar-current! hv old)
  (set-hvar-history! hv hist))

;; unit tests
(define hv (make-hvar 0))
(hvar-set! hv 1)
(check-equal? (hvar-current hv) 1)
(hvar-set! hv 2)
(hvar-set! hv 3)
(check-equal? (hvar-history hv) '(2 1 0))
(hvar-undo! hv)
(hvar-undo! hv)
(hvar-undo! hv)
(check-equal? (hvar-current hv) 0)
