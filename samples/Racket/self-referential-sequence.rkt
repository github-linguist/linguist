#lang racket

(define (next s)
  (define v (make-vector 10 0))
  (for ([c s])
    (define d (- (char->integer #\9) (char->integer c)))
    (vector-set! v d (add1 (vector-ref v d))))
  (string-append* (for/list ([x v] [i (in-range 9 -1 -1)] #:when (> x 0))
                    (format "~a~a" x i))))

(define (seq-of s)
  (reverse (let loop ([ns (list s)])
             (define n (next (car ns)))
             (if (member n ns) ns (loop (cons n ns))))))

(define (sort-string s)
  (list->string (sort (string->list s) char>?)))

(define-values [len nums seq]
  (for/fold ([*len #f] [*nums #f] [*seq #f])
            ([n (in-range 1000000 -1 -1)]) ; start at the high end
    (define s (number->string n))
    (define sorted (sort-string s))
    (cond [(equal? s sorted)
           (define seq (seq-of s))
           (define len (length seq))
           (cond [(or (not *len) (> len *len)) (values len (list s) seq)]
                 [(= len *len) (values len (cons s *nums) seq)]
                 [else (values *len *nums *seq)])]
          ;; not sorted: see if it's a permutation of the best
          [else (values
                 *len
                 (if (and *nums (member sorted *nums)) (cons s *nums) *nums)
                 *seq)])))
(printf "Numbers: ~a\nLength: ~a\n" (string-join nums ", ") len)
(for ([n seq]) (printf "  ~a\n" n))
