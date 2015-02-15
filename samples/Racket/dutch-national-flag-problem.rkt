#lang racket

(define dutch-colors '(red white blue))

(define (dutch-order? balls)
  ;; drop each color from the front, should end up empty
  (null? (for/fold ([r balls]) ([color dutch-colors])
           (dropf r (curry eq? color)))))

(define (random-balls)
  (define balls
    (for/list ([i (random 20)])
      (list-ref dutch-colors (random (length dutch-colors)))))
  (if (dutch-order? balls) (random-balls) balls))

;; first method: use a key to map colors to integers
(define (order->key order)
  (let ([alist (for/list ([x order] [i (in-naturals)]) (cons x i))])
    (λ(b) (cdr (assq b alist)))))
(define (sort-balls/key balls)
  (sort balls < #:key (order->key dutch-colors)))

;; second method: use a comparator built from the ordered list
(define ((order<? ord) x y)
  (memq y (cdr (memq x ord))))
(define (sort-balls/compare balls)
  (sort balls (order<? dutch-colors)))

(define (test sort)
  (define balls (random-balls))
  (define sorted (sort balls))
  (printf "Testing ~a:\n  Random: ~s\n  Sorted: ~s\n      ==> ~s\n"
          (object-name sort)
          balls sorted (if (dutch-order? sorted) 'OK 'BAD)))
(for-each test (list sort-balls/key sort-balls/compare))
