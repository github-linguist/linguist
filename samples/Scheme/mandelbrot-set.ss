(define x-centre -0.5)
(define y-centre 0.0)
(define width 4.0)
(define i-max 800)
(define j-max 600)
(define n 100)
(define r-max 2.0)
(define file "out.pgm")
(define colour-max 255)
(define pixel-size (/ width i-max))
(define x-offset (- x-centre (* 0.5 pixel-size (+ i-max 1))))
(define y-offset (+ y-centre (* 0.5 pixel-size (+ j-max 1))))

(define (inside? z)
  (define (*inside? z-0 z n)
    (and (< (magnitude z) r-max)
         (or (= n 0)
             (*inside? z-0 (+ (* z z) z-0) (- n 1)))))
  (*inside? z 0 n))

(define (boolean->integer b)
  (if b colour-max 0))

(define (pixel i j)
  (boolean->integer
    (inside?
      (make-rectangular (+ x-offset (* pixel-size i))
                        (- y-offset (* pixel-size j))))))

(define (plot)
  (with-output-to-file file
    (lambda ()
      (begin (display "P2") (newline)
             (display i-max) (newline)
             (display j-max) (newline)
             (display colour-max) (newline)
             (do ((j 1 (+ j 1))) ((> j j-max))
                 (do ((i 1 (+ i 1))) ((> i i-max))
                     (begin (display (pixel i j)) (newline))))))))

(plot)
