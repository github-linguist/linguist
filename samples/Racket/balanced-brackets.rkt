#lang racket

(define (generate n)
  (list->string (shuffle (append* (make-list n '(#\[ #\]))))))

(define (balanced? str)
  (let loop ([l (string->list str)] [n 0])
    (or (null? l)
        (if (eq? #\[ (car l))
          (loop (cdr l) (add1 n))
          (and (> n 0) (loop (cdr l) (sub1 n)))))))

(define (try n)
  (define s (generate n))
  (printf "~a => ~a\n" s (if (balanced? s) "OK" "NOT OK")))

(for ([n 10]) (try n))
