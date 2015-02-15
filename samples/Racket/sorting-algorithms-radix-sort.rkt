#lang racket

(require data/queue)

(define (radix-sort l r)
  (define queues (for/vector #:length r ([_ r]) (make-queue)))
  (let loop ([l l] [R 1])
    (define all-zero? #t)
    (for ([x (in-list l)])
      (define x/R (quotient x R))
      (enqueue! (vector-ref queues (modulo x/R r)) x)
      (unless (zero? x/R) (set! all-zero? #f)))
    (if all-zero? l
        (loop (let q-loop ([i 0])
                (define q (vector-ref queues i))
                (let dq-loop ()
                  (if (queue-empty? q)
                    (if (< i (sub1 r)) (q-loop (add1 i)) '())
                    (cons (dequeue! q) (dq-loop)))))
              (* R r)))))

(for/and ([i 10000]) ; run some tests on random lists with a random radix
  (define (make-random-list)
    (for/list ([i (+ 10 (random 10))]) (random 100000)))
  (define (sorted? l)
    (match l [(list) #t] [(list x) #t]
           [(list x y more ...) (and (<= x y) (sorted? (cons y more)))]))
  (sorted? (radix-sort (make-random-list) (+ 2 (random 98)))))
;; => #t, so all passed
