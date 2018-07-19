#lang racket

;; Using short names to make the code line up nicely
(struct N (color left value right) #:prefab)

(define (balance t)
  (match t
    [(N 'B (N 'R (N 'R a x b) y c) z d) (N 'R (N 'B a x b) y (N 'B c z d))]
    [(N 'B (N 'R a x (N 'R b y c)) z d) (N 'R (N 'B a x b) y (N 'B c z d))]
    [(N 'B a x (N 'R (N 'R b y c) z d)) (N 'R (N 'B a x b) y (N 'B c z d))]
    [(N 'B a x (N 'R b y (N 'R c z d))) (N 'R (N 'B a x b) y (N 'B c z d))]
    [else t]))

(define (insert x s)
  (define (ins t)
    (match t
      ['empty (N 'R 'empty x 'empty)]
      [(N c l v r) (cond [(< x v) (balance (N c (ins l) v r))]
                         [(> x v) (balance (N c l v (ins r)))]
                         [else t])]))
  (match (ins s) [(N _ l v r) (N 'B l v r)]))

(define (visualize t0)
  (let loop ([t t0] [last? #t] [indent '()])
    (define (I mid last) (cond [(eq? t t0) ""] [last? mid] [else last]))
    (for-each display (reverse indent))
    (printf "~a~a[~a]\n" (I "\\-" "+-") (N-value t) (N-color t))
    (define subs (filter N? (list (N-left t) (N-right t))))
    (for ([s subs] [n (in-range (sub1 (length subs)) -1 -1)])
      (loop s (zero? n) (cons (I "  " "| ") indent)))))

(visualize (for/fold ([t 'empty]) ([i 16]) (insert i t)))
