(define (catalan m)
    (let loop ((c 1)(n 0))
        (if (not (eqv? n m))
            (begin
                (display n)(display ": ")(display c)(newline)
                (loop (* (/ (* 2 (- (* 2 (+ n 1)) 1)) (+ (+ n 1) 1)) c) (+ n 1) )))))

(catalan 15)
