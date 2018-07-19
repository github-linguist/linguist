#lang racket

;; Both assume valid inputs
(define (num->str N r)
  (let loop ([N N] [digits '()])
    (define-values [N1 d] (quotient/remainder N r))
    (define digits1 (cons (integer->char (+ d (if (< d 10) 48 55))) digits))
    (if (zero? N) (list->string digits1) (loop N1 digits1))))
(define (str->num S r)
  (for/fold ([N 0])
            ([B (string->bytes/utf-8 (string-upcase S))])
    (+ (* N r) (- B (if (< 64 B) 55 48)))))

;; To try it out:
(define (random-test)
  (define N (random 1000000))
  (define r (+ 2 (random 35)))
  (define S (num->str N r))
  (define M (str->num S r))
  (printf "~s -> ~a#~a -> ~a => ~a\n" N S r M (if (= M N) 'OK 'BAD)))
;; (random-test)
