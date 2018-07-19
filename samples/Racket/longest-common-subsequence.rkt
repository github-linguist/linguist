#lang racket
(define (longest xs ys)
  (if (> (length xs) (length ys))
      xs ys))

(define memo (make-hash))
(define (lookup xs ys)
  (hash-ref memo (cons xs ys) #f))
(define (store xs ys r)
  (hash-set! memo (cons xs ys) r)
  r)

(define (lcs/list sx sy)
  (or (lookup sx sy)
      (store sx sy
             (match* (sx sy)
               [((cons x xs) (cons y ys))
                (if (equal? x y)
                    (cons x (lcs/list xs ys))
                    (longest (lcs/list sx ys) (lcs/list xs sy)))]
               [(_ _) '()]))))

(define (lcs sx sy)
  (list->string (lcs/list (string->list sx) (string->list sy))))

(lcs "thisisatest" "testing123testing")
