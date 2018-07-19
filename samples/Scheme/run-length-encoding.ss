(define (run-length-decode v)
   (apply string-append (map (lambda (p) (make-string (car p) (cdr p))) v)))

(define (run-length-encode s)
(let ((n (string-length s)))
(let loop ((i (- n 2)) (c (string-ref s (- n 1))) (k 1) (v '()))
(if (negative? i) (cons (cons k c) v)
    (let ((x (string-ref s i)))
    (if (char=? c x) (loop (- i 1) c (+ k 1) v)
                     (loop (- i 1) x 1 (cons (cons k c) v))))))))

(run-length-encode "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW")
; ((12 . #\W) (1 . #\B) (12 . #\W) (3 . #\B) (24 . #\W) (1 . #\B) (14 . #\W))
(run-length-decode '((12 . #\W) (1 . #\B) (12 . #\W) (3 . #\B) (24 . #\W) (1 . #\B) (14 . #\W)))
; "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
