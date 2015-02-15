#lang racket

(struct item (name explanation value weight volume) #:prefab)

(define items
  (list
   (item "panacea (vials of)" "Incredible healing properties" 3000 0.3 0.025)
   (item "ichor (ampules of)" "Vampires blood"                1800 0.2 0.015)
   (item "gold (bars)"        "Shiney shiney"                 2500 2.0 0.002)))

(define (fill-sack items volume-left weight-left sack sack-value)
  (match items
    ['() (values (list sack) sack-value)]
    [(cons (and (item _ _ item-val weight volume) item) items)
     (define max-q-wgt (floor (/ weight-left weight)))
     (define max-q-vol (floor (/ volume-left volume)))
     (for/fold ([best (list sack)] [best-val sack-value])
               ([n (exact-round (add1 (min max-q-vol max-q-wgt)))])
       (define-values [best* best-val*]
         (fill-sack items
                    (- volume-left (* n volume))
                    (- weight-left (* n weight))
                    (cons (cons n item) sack)
                    (+ sack-value (* n item-val))))
       (cond [(> best-val* best-val) (values best* best-val*)]
             [(= best-val* best-val) (values (append best best*) best-val*)]
             [else                   (values best best-val)]))]))

(define (display-sack sack total)
  (for ([sk sack])
    (define qty (car sk))
    (define name (item-name (cdr sk)))
    (if (zero? qty)
      (printf "Leave ~a\n" name)
      (printf "Take ~a ~a\n" qty name)))
  (printf "GRAND TOTAL: ~a\n\n" total))

(call-with-values (λ() (fill-sack items 0.25 25 '() 0))
                  (λ(sacks total) (for ([s sacks]) (display-sack s total))))
