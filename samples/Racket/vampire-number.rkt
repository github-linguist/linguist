#lang racket

;; chock full of fun... including divisors
(require math/number-theory)

;; predicate to tell if n is a vampire number
(define (sub-vampire?-and-fangs n)
  (define digit-count-n (add1 (order-of-magnitude n)))
  (define (string-sort-characters s) (sort (string->list s) char<?))
  (define digits-in-order-n (string-sort-characters (number->string n)))
  (define (fangs-of-n? d e)
    (and (<= d e) ; avoid duplication
         (= (add1 (order-of-magnitude d)) (add1 (order-of-magnitude e)) (/ digit-count-n 2))
         (not (= 0 (modulo d 10) (modulo e 10)))
         (equal? digits-in-order-n
                 (string-sort-characters (string-append (number->string d) (number->string e))))))

  (let* ((fangses (for*/list ((d (in-list (divisors n))) #:when (fangs-of-n? d (/ n d)))
                    (list d (/ n d)))))
    (and (not (null? fangses)) (cons n fangses))))

(define (vampire?-and-fangs n)
  (and (odd? (order-of-magnitude n)) ; even number of digits - else not even worth looking!
       (sub-vampire?-and-fangs n)))

(displayln "First 25 vampire numbers:")
(for ((vmp (sequence-filter identity (sequence-map vampire?-and-fangs (in-naturals 1))))
      (cnt (in-range 1 (add1 25))))
  (printf "#~a ~a~%" cnt vmp))

(displayln "Test the big numbers:")
(displayln (vampire?-and-fangs 16758243290880))
(displayln (vampire?-and-fangs 24959017348650))
(displayln (vampire?-and-fangs 14593825548650))
