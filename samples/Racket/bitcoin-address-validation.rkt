#lang racket/base

;; Same sha-256 interface as the same-named task
(require ffi/unsafe ffi/unsafe/define openssl/libcrypto)
(define-ffi-definer defcrypto libcrypto)
(defcrypto SHA256_Init   (_fun _pointer -> _int))
(defcrypto SHA256_Update (_fun _pointer _pointer _long -> _int))
(defcrypto SHA256_Final  (_fun _pointer _pointer -> _int))
(define (sha256 bytes)
  (define ctx (malloc 128))
  (define result (make-bytes 32))
  (SHA256_Init ctx)
  (SHA256_Update ctx bytes (bytes-length bytes))
  (SHA256_Final result ctx)
  result)

;; base58 decoding
(define base58-digits
  (let ([v (make-vector 128 #f)])
    (for ([i (in-naturals)]
          [c "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"])
      (vector-set! v (char->integer c) i))
    v))
(define (base58->integer str)
  (for/fold ([n 0]) ([c str])
    (+ (* n 58) (vector-ref base58-digits (char->integer c)))))

(define (int->bytes n digits)
  (list->bytes (let loop ([n n] [digits digits] [acc '()])
                 (if (zero? digits) acc
                     (let-values ([(q r) (quotient/remainder n 256)])
                       (loop q (sub1 digits) (cons r acc)))))))

(define (validate-bitcoin-address str)
  (define bs (int->bytes (base58->integer str) 25))
  (equal? (subbytes (sha256 (sha256 (subbytes bs 0 21))) 0 4)
          (subbytes bs 21)))

;; additional tests taken from the other solutions
(validate-bitcoin-address "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i") ; => #t
(validate-bitcoin-address "1111111111111111111114oLvT2")        ; => #t
(validate-bitcoin-address "17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j") ; => #t
(validate-bitcoin-address "1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9") ; => #t
(validate-bitcoin-address "1badbadbadbadbadbadbadbadbadbadbad") ; => #f
