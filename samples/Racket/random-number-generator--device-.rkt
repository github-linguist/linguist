#lang racket
;; Assuming a device to provide random bits:
(call-with-input-file* "/dev/random"
  (λ(i) (integer-bytes->integer (read-bytes 4 i) #f)))
