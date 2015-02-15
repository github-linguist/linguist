#lang racket

(require ffi/unsafe)

(define (_autobitmask l)
  (_bitmask (append* (for/list ([x l] [i (in-naturals)]) `(,x = ,(expt 2 i))))))

(define _rs232 (_autobitmask '(CD RD TD DTR SG DSR RTS CTS RI )))

;; Usually it will get used when using foreign functions automatically, but
;; this demonstrates the conversions explicitly
(require (only-in '#%foreign ctype-scheme->c ctype-c->scheme))
((ctype-scheme->c _rs232) '(SG TD RI)) ; -> 276
((ctype-c->scheme _rs232) 276)         ; -> '(TD SG RI)
