#lang racket

(struct bucket (value [lock #:auto])
  #:auto-value #f
  #:mutable
  #:transparent)

(define *buckets* (build-vector 10 (λ (i) (bucket 100))))

(define (show-buckets)
  (let* ([values (for/list ([b *buckets*]) (bucket-value b))]
         [total (apply + values)])
    (append values (list '- total))))

(define *equalizations* 0)
(define *randomizations* 0)
(define *blocks* 0)

(define (show-stats)
  (let ([n (length *log*)]
        [log (reverse *log*)])
    (printf "Equalizations ~a, Randomizations ~a, Transfers: ~a, Blocks ~a\n"
              *equalizations* *randomizations* n *blocks*)
    (for ([i (in-range 10)])
      (define j (min (floor (* i (/ n 9))) (sub1 n)))
      (printf "~a (~a). " (add1 i) (add1 j))
      (displayln (list-ref log j)))))

(define *log* (list (show-buckets)))

(define-syntax-rule (inc! x) (set! x (add1 x)))

(define (get-bucket i) (vector-ref *buckets* i))

(define (get-value i) (bucket-value (get-bucket i)))
(define (set-value! i v) (set-bucket-value! (get-bucket i) v))

(define (locked? i) (bucket-lock (vector-ref *buckets* i)))
(define (lock! i v) (set-bucket-lock! (get-bucket i) v))
(define (unlock! i) (lock! i #f))

(define *clamp-lock* #f)

(define (clamp i j)
  (cond [*clamp-lock* (inc! *blocks*)
                      #f]
        [else (set! *clamp-lock* #t)
              (let ([result #f]
                    [g (gensym)])
                (unless (locked? i)
                  (lock! i g)
                  (cond [(locked? j) (unlock! i)]
                        [else (lock! j g)
                              (set! result #t)]))
                (unless result (inc! *blocks*))
                (set! *clamp-lock* #f)
                result)]))

(define (unclamp i j)
  (unlock! i)
  (unlock! j))

(define (transfer i j amount)
  (let* ([lock1 (locked? i)]
         [lock2 (locked? j)]
         [a (get-value i)]
         [b (get-value j)]
         [c (- a amount)]
         [d (+ b amount)])
    (cond [(< c 0) (error 'transfer "Removing too much.")]
          [(< d 0) (error 'transfer "Stealing too much.")]
          [(and lock1 (equal? lock1 lock2)) (set-value! i c)
                                            (set-value! j d)
                                            (set! *log*
                                                  (cons (show-buckets) *log*))]
          [else (error 'transfer "Lock problem")])))

(define (equalize i j)
  (when (clamp i j)
    (let ([a (get-value i)]
          [b (get-value j)])
      (unless (= a b)
        (transfer i j (if (> a b)
                          (floor (/ (- a b) 2))
                          (- (floor (/ (- b a) 2)))))
        (inc! *equalizations*)))
        (unclamp i j)))

(define (randomize i j)
  (when (clamp i j)
    (let* ([a (get-value i)]
           [b (get-value j)]
           [t (+ a b)]
           [r (if (= t 0) 0 (random t))])
      (unless (= r 0)
        (transfer i j (- a r))
        (inc! *randomizations*)))
    (unclamp i j)))

(thread (λ () (for ([_ (in-range 500000)]) (equalize (random 10) (random 10)))))
(thread (λ () (for ([_ (in-range 500000)]) (randomize (random 10) (random 10)))))
