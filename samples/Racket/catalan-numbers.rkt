#lang racket
(require planet2)
; (install "this-and-that")  ; uncomment to install
(require memoize/memo)

(define/memo* (catalan m)
  (if (= m 0)
      1
      (for/sum ([i m])
        (* (catalan i) (catalan (- m i 1))))))

(map catalan (range 1 15))
