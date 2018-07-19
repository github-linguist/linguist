#lang racket

(define (try n)
  (printf "Original number: ~s (0x~x)\n" n n)
  (define 4octets (integer->integer-bytes n 4 #f))
  (printf "Octets: ~a (byte-string: ~s)\n"
          (string-join (map (λ(o) (~r o #:base 16))
                            (bytes->list 4octets))
                       ":")
          4octets)
  (define m (integer-bytes->integer 4octets #f))
  (printf "Back to a number: ~s (~a)\n"
          m (if (= m n) "OK" "BAD")))

(for-each try '(#x200000 #x1fffff))
