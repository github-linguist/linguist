(define s "Hello, world!")
(define n 5)
(define m (+ n 6))

(display (substring s n m))
(newline)

(display (substring s n))
(newline)

(display (substring s 0 (- (string-length s) 1)))
(newline)

(display (substring s (string-index s #\o) m))
(newline)

(display (substring s (string-contains s "lo") m))
(newline)
