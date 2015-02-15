#lang racket/base

(define-syntax-rule (inc! x)
  (set! x (add1 x)))

(define (permutation-test control-gr treatment-gr)
  (let ([both-gr (append control-gr treatment-gr)]
        [threshold (apply + control-gr)]
        [more 0]
        [leq 0])
    (let loop ([data both-gr] [sum 0] [needed (length control-gr)] [available (length both-gr)])
      (cond [(zero? needed) (if (>= sum threshold)
                                (inc! more)
                                (inc! leq))]
            [(>= available needed) (loop (cdr data) sum needed (sub1 available))
                                   (loop (cdr data) (+ sum (car data)) (sub1 needed) (sub1 available))]
            [else (void)]))
    (values more leq)))

(let-values ([(more leq) (permutation-test '(68 41 10 49 16 65 32 92 28 98)
                                           '(85 88 75 66 25 29 83 39 97))])
  (let ([sum (+ more leq)])
    (printf "<=: ~a ~a%~n>:  ~a ~a%~n"
            more (real->decimal-string (* 100. (/ more sum)) 2)
            leq (real->decimal-string (* 100. (/ leq sum)) 2))))
