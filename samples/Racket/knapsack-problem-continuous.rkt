#lang racket
(define shop-inventory
  '((beef 	3.8 	36)
    (pork 	5.4 	43)
    (ham 	3.6 	90)
    (greaves 	2.4 	45)
    (flitch 	4.0 	30)
    (brawn 	2.5 	56)
    (welt 	3.7 	67)
    (salami 	3.0 	95)
    (sausage 	5.9 	98)))


(define (continuous-knapsack shop sack sack-capacity sack-total-value)
  ;; solved by loading up on the highest value item...
  (define (value/kg item) (/ (third item) (second item)))
  (if (zero? sack-capacity)
      (values (reverse sack) sack-total-value)
      (let* ((best-value-item (argmax value/kg shop))
             (bvi-full-weight (second best-value-item))
             (amount-can-take (min sack-capacity bvi-full-weight))
             (bvi-full-value  (third best-value-item))
             (bvi-taken-value (* bvi-full-value (/ amount-can-take bvi-full-weight))))
        (continuous-knapsack (remove best-value-item shop)
                             (cons (list (first best-value-item)
                                         (if (= amount-can-take bvi-full-weight)
                                             'all-of amount-can-take) bvi-taken-value)
                                   sack)
                             (- sack-capacity amount-can-take)
                             (+ sack-total-value bvi-taken-value)))))

(define (report-knapsack sack total-value)
  (for-each (lambda (item)
              (if (eq? 'all-of (second item))
                  (printf "Take all of the ~a (for ~a)~%"
                          (first item) (third item))
                  (printf "Take ~a of the ~a (for ~a)~%"
                          (real->decimal-string (second item))
                          (first item)
                          (real->decimal-string (third item)))))
            sack)
  (printf "For a grand total of: ~a" (real->decimal-string total-value)))

(call-with-values (lambda () (continuous-knapsack shop-inventory null 15 0))
                  report-knapsack)
