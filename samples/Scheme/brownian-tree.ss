; Save bitmap to external file
(define (save-pbm bitmap filename)
	(define f (open-output-file filename))
	(simple-format f "P1\n~A ~A\n"
		(list-ref (array-dimensions bitmap) 0)
		(list-ref (array-dimensions bitmap) 1))
	(do ((c 0 (+ c 1))) ((eqv? c (list-ref (array-dimensions bitmap) 1)))
	(do ((r 0 (+ r 1))) ((eqv? r (list-ref (array-dimensions bitmap) 0)))
		(display (array-ref bitmap r c) f))
		(newline f))
	(close-output-port f)
)

; Return a random coordinate in the bitmap that isn't filled yet along with a direction
(define (new-particle bitmap)
	(define x (random (list-ref (array-dimensions bitmap) 0)))
	(define y (random (list-ref (array-dimensions bitmap) 1)))
	(define dx (- (random 3) 1))
	(define dy (- (random 3) 1))
	;Repeat until we find an unused location
	(if (> (array-ref bitmap x y) 0)
		(new-particle bitmap)
		(list (list x y) (list dx dy))))

; Check neighboring coordinates to see if a collision occured
(define (collision-check bitmap p)
	(define c #f)
	(define oob #f)
	(define x (list-ref (car p) 0))
	(define y (list-ref (car p) 1))
	(define dx (list-ref (cadr p) 0))
	(define dy (list-ref (cadr p) 1))
	(define w (list-ref (array-dimensions bitmap) 0))
	(define h (list-ref (array-dimensions bitmap) 1))
	
	; If the particle hasn't gone out of bounds keep checking for a collision
	(if (or (> 0 x) (> 0 y) (<= w x) (<= h y))
		(set! oob #t)
		(do ((x (- (list-ref (car p) 0) 1) (+ x 1))) ((eqv? x (+ (list-ref (car p) 0) 2)))
		(do ((y (- (list-ref (car p) 1) 1) (+ y 1))) ((eqv? y (+ (list-ref (car p) 1) 2)))
			; Check existing neighbors for collisions
			(if (and (<= 0 x) (<= 0 y) (> w x) (> h y))
				(if (not (zero? (array-ref bitmap x y)))
					(set! c #t))))))
	(if oob
		#f	; Return false if out of bounds
		(if c
			p ; Return the point of collision if a collision occured
			(if (and (zero? dx) (zero? dy))
				#f ; Return false if particle is motionless with no collision
				(collision-check bitmap (particle-move p))))))

; Plot a particle on the bitmap
(define (particle-plot! bitmap p)
	(array-set! bitmap 1 (list-ref (car p) 0) (list-ref (car p) 1)))

; Move a particle along its slope
(define (particle-move p)
	(list (list
		(+ (list-ref (car p) 0) (list-ref (cadr p) 0))
		(+ (list-ref (car p) 1) (list-ref (cadr p) 1)))
		(cadr p)))

; Grow a brownian tree
(define (grow-brownian-tree! bitmap collisions)
	(define w (list-ref (array-dimensions bitmap) 0))
	(define h (list-ref (array-dimensions bitmap) 1))

	; Generate a new particle at a random location
	(define p (new-particle bitmap))

	; Find a collision or lack of one and plot it on the bitmap
	(set! p (collision-check bitmap p))
	(if p (begin
			; Display collision number and the place it happened
			(display collisions)(display ": ")(display (car p))(newline)
			(set! collisions (- collisions 1))
			; Plot the point
			(particle-plot! bitmap p)))

	; If we're done say so
	(if (zero? collisions)
		(display "Done\n"))

	; Keep going until we have enough collisions
	; or have filled the bitmap
	(if (and (< 0 collisions) (memq 0 (array->list (array-contents bitmap))))
		(grow-brownian-tree! bitmap collisions)))

; Plot a random point to seed the brownian tree
(define (seed-brownian-tree! bitmap)
	(define p (new-particle bitmap))
	(particle-plot! bitmap p))

;;; Example usage ;;;
; Seed the random number generator
(let ((time (gettimeofday)))
	(set! *random-state*
		(seed->random-state (+ (car time) (cdr time)))))

; Generate a tree with 320*240 collisions on a bitmap of the size 640x480
; The bitmap is zeroed to start and written with a one where a collision occurs
(define bitmap (make-array 0 640 480))
(seed-brownian-tree! bitmap)
(grow-brownian-tree! bitmap (* 320 240))

; Save to a portable bitmap file
(save-pbm bitmap "brownian-tree.pbm")
