#lang racket
(require ffi/unsafe)
(define libm (ffi-lib "libm")) ; get a handle for the C math library
; look up sqrt in the math library. if we can't find it, return the builtin sqrt
(define extern-sqrt (get-ffi-obj 'sqrt libm (_fun _double -> _double)
                                 (lambda () sqrt)))
