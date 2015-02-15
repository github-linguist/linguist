#lang racket/base

(define (s-of-n-creator n)
  (let ([count 0] ; 'i' in the description
         [vec (make-vector n)]) ; store the elts we've seen so far
    (lambda (item)
      (if (< count n)
          ; we're not full, so, kind of boring
          (begin
            (vector-set! vec count item)
            (set! count (+ count 1)))
          ; we've already seen n elts; fun starts
          (begin
            (set! count (+ count 1))
            (when (< (random count) n)
              (vector-set! vec (random n) item))))
      vec)))

(define counts (make-vector 10))

(for ([iter (in-range 0 100000)]) ; trials
  (let ([s-of-n (s-of-n-creator 3)]) ; set up the chooser
    (for ([d (in-vector ; iterate over the chosen digits
              (for/last ([digit (in-range 0 10)]) ; loop through the digits
                        (s-of-n digit)))]) ; feed them in
      (vector-set! counts d (add1 (vector-ref counts d)))))) ; update counts

(for ([d (in-range 0 10)])
  (printf "~a ~a~n" d (vector-ref counts d)))
