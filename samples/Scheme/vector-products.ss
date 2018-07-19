(define (dot-product A B)
    (apply + (map * (vector->list A) (vector->list B))))

(define (cross-product A B)
	(define len (vector-length A))
	(define xp (make-vector (vector-length A) #f))
	(let loop ((n 0))
		(vector-set! xp n (-
			(* (vector-ref A (modulo (+ n 1) len))
				(vector-ref B (modulo (+ n 2) len)))
			(* (vector-ref A (modulo (+ n 2) len))
				(vector-ref B (modulo (+ n 1) len)))))
		(if (eqv? len (+ n 1))
			xp
			(loop (+ n 1)))))

(define (scalar-triple-product A B C)
	(dot-product A (cross-product B C)))

(define (vector-triple-product A B C)
	(cross-product A (cross-product B C)))


(define A #( 3 4 5))
(define B #(4 3 5))
(define C #(-5 -12 -13))

(display "A = ")(display A)(newline)
(display "B = ")(display B)(newline)
(display "C = ")(display C)(newline)
(newline)
(display "A . B = ")(display (dot-product A B))(newline)
(display "A x B = ")(display (cross-product A B))(newline)
(display "A . B x C = ")(display (scalar-triple-product A B C))(newline)
(display "A x B x C = ") (display (vector-triple-product A B C))(newline)
