(define (quadratic a b c)
	(if (= a 0)
	(if (= b 0) 'fail (- (/ c b)))
	(let ((delta (- (* b b) (* 4 a c))))
	(if (and (real? delta) (> delta 0))
		(let ((u (+ b (* (if (>= b 0) 1 -1) (sqrt delta)))))
			(list (/ u -2 a) (/ (* -2 c) u)))
		(list
			(/ (- (sqrt delta) b) 2 a)
			(/ (+ (sqrt delta) b) -2 a))))))


; examples

(quadratic 1 -1 -1)
; (1.618033988749895 -0.6180339887498948)

(quadratic 1 0 -2)
; (-1.4142135623730951 1.414213562373095)

(quadratic 1 0 2)
; (0+1.4142135623730951i 0-1.4142135623730951i)

(quadratic 1+1i 2 5)
; (-1.0922677260818898-1.1884256155834088i 0.09226772608188982+2.1884256155834088i)

(quadratic 0 4 3)
; -3/4

(quadratic 0 0 1)
; fail

(quadratic 1 2 0)
; (-2 0)

(quadratic 1 2 1)
; (-1 -1)

(quadratic 1 -1e5 1)
; (99999.99999 1.0000000001000001e-05)
