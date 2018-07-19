;
; Works with R7RS-compatible Schemes (e.g. Chibi).
; Also current versions of Chicken, Gauche and Kawa.
;
(cond-expand
  (chicken (use srfi-13))
  (gauche  (use srfi-13))
  (kawa    (import (srfi :13)))
  (else    (import (scheme base) (scheme write)))) ; R7RS


(define msg "The quick brown fox jumps over the lazy dog.")
(define key 13)

(define (caesar char)
  (define A (char->integer #\A))
  (define Z (char->integer #\Z))
  (define a (char->integer #\a))
  (define z (char->integer #\z))
  (define c (char->integer char))
  (integer->char
    (cond ((<= A c Z) (+ A (modulo (+ key (- c A)) 26)))
          ((<= a c z) (+ a (modulo (+ key (- c a)) 26)))
          (else c)))) ; Return other characters verbatim.

(display (string-map caesar msg))
(newline)
