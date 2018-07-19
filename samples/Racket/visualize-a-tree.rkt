#lang racket/base

(define (visualize t0)
  (let loop ([t t0] [last? #t] [indent '()])
    (define (I mid last) (cond [(eq? t t0) ""] [last? mid] [else last]))
    (for-each display (reverse indent))
    (unless (eq? t t0) (printf "|\n"))
    (for-each display (reverse indent))
    (printf "~a~a\n" (I "\\-" "+-") (car t))
    (for ([s (cdr t)] [n (in-range (- (length t) 2) -1 -1)])
      (loop s (zero? n) (cons (I "  " "| ") indent)))))

(visualize '(1 (2 (3 (4) (5) (6 (7))) (8 (9)) (10)) (11 (12) (13))))
