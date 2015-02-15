#lang racket

(define smalls
  (map symbol->string
       '(zero one two three four five six seven eight nine ten eleven twelve
         thirteen fourteen fifteen sixteen seventeen eighteen nineteen)))

(define tens
  (map symbol->string
       '(zero ten twenty thirty forty fifty sixty seventy eighty ninety)))

(define larges
  (map symbol->string
       '(thousand million billion trillion quadrillion quintillion sextillion
         septillion octillion nonillion decillion undecillion duodecillion
         tredecillion quattuordecillion quindecillion sexdecillion
         septendecillion octodecillion novemdecillion vigintillion)))

(define (integer->english n)
  (define (step div suffix separator [subformat integer->english])
    (define-values [q r] (quotient/remainder n div))
    (define S (if suffix (~a (subformat q) " " suffix) (subformat q)))
    (if (zero? r) S (~a S separator (integer->english r))))
  (cond [(< n 0) (~a "negative " (integer->english (- n)))]
        [(< n 20) (list-ref smalls n)]
        [(< n 100) (step 10 #f "-" (curry list-ref tens))]
        [(< n 1000) (step 100 "hundred" " and ")]
        [else (let loop ([N 1000000] [D 1000] [unit larges])
                (cond [(null? unit)
                       (error 'integer->english "number too big: ~e" n)]
                      [(< n N) (step D (car unit) ", ")]
                      [else (loop (* 1000 N) (* 1000 D) (cdr unit))]))]))

(for ([n 10])
  (define e (expt 10 n))
  (define r (+ (* e (random e)) (random e)))
  (printf "~s: ~a\n" r (integer->english r)))
