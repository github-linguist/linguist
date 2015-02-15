(define (halve num)
  (quotient num 2))

(define (double num)
  (* num 2))

(define (*mul-eth plier plicand acc)
  (cond ((zero? plier) acc)
        ((even? plier) (*mul-eth (halve plier) (double plicand) acc))
        (else (*mul-eth (halve plier) (double plicand) (+ acc plicand)))))

(define (mul-eth plier plicand)
  (*mul-eth plier plicand 0))

(display (mul-eth 17 34))
(newline)
