#lang racket

;; Translation of the pseudo code
(define (gnome-sort1 a <=?)
  (define size (vector-length a))
  (define (swap i j)
    (define t (vector-ref a i))
    (vector-set! a i (vector-ref a j))
    (vector-set! a j t))
  (let loop ([i 1] [j 2])
    (when (< i size)
      (if (<=? (vector-ref a (sub1 i)) (vector-ref a i))
        (loop j (add1 j))
        (begin (swap (sub1 i) i)
               (let ([i (sub1 i)])
                 (if (zero? i)
                   (loop j (add1 j))
                   (loop i j)))))))
  a)
(gnome-sort1 (vector 3 2 1 4 5 6) <=)

;; a functional version, roughly like the Scheme entry
(define (gnome-sort2 l <=?)
  (match l
    [(list) l]
    [(list x xs ...)
     (let loop ([x `((,x) . ,xs)])
       (match x
         [`(,ps) ps]
         [`((,p . ,ps) ,n . ,ns)
          (loop (cond [(<=? n p)  `((,n ,p . ,ps) . ,ns)]
                      [(null? ps) `((,n) ,p . ,ns)]
                      [else       `(,ps ,n ,p . ,ns)]))]))]))
(gnome-sort2 '(3 2 1 4 5 6) <=)
