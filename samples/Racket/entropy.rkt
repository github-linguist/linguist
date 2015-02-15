#lang racket
(require math)
(provide entropy hash-entropy list-entropy digital-entropy)

(define (hash-entropy h)
  (define (log2 x) (/ (log x) (log 2)))
  (define n (for/sum [(c (in-hash-values h))] c))
  (- (for/sum ([c (in-hash-values h)] #:unless (zero? c))
       (* (/ c n) (log2 (/ c n))))))

(define (list-entropy x) (hash-entropy (samples->hash x)))

(define entropy         (compose list-entropy string->list))
(define digital-entropy (compose entropy number->string))

(module+ test
  (require rackunit)
  (check-= (entropy "1223334444") 1.8464393446710154 1E-8)
  (check-= (digital-entropy 1223334444) (entropy "1223334444") 1E-8)
  (check-= (digital-entropy 1223334444) 1.8464393446710154 1E-8)
  (check-= (entropy "xggooopppp") 1.8464393446710154 1E-8))

(module+ main (entropy "1223334444"))
