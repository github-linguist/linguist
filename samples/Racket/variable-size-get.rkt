#lang racket
(require ffi/unsafe)
(define-syntax-rule (sizes t ...)
  (begin (printf "sizeof(~a) = ~a\n" 't (ctype-sizeof t)) ...))
(sizes _byte _short _int _long _llong _float _double)
