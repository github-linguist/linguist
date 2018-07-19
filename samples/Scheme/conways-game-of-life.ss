;;An R6RS Scheme implementation of Conway's Game of Life --- assumes
;;all cells outside the defined grid are dead

;if n is outside bounds of list, return 0 else value at n
(define (nth n lst)
  (cond ((> n (length lst)) 0)
        ((< n 1) 0)
        ((= n 1) (car lst))
        (else (nth (- n 1) (cdr lst)))))

;return the next state of the supplied universe
(define (next-universe universe)
  ;value at (x, y)
  (define (cell x y)
    (if (list? (nth y universe))
        (nth x (nth y universe))
        0))
  ;sum of the values of the cells surrounding (x, y)
  (define (neighbor-sum x y)
    (+ (cell (- x 1) (- y 1))
       (cell (- x 1) y)
       (cell (- x 1) (+ y 1))
       (cell x (- y 1))
       (cell x (+ y 1))
       (cell (+ x 1) (- y 1))
       (cell (+ x 1) y)
       (cell (+ x 1) (+ y 1))))
  ;next state of the cell at (x, y)
  (define (next-cell x y)
    (let ((cur (cell x y))
          (ns (neighbor-sum x y)))
      (cond ((and (= cur 1)
                  (or (< ns 2) (> ns 3)))
             0)
            ((and (= cur 0) (= ns 3))
             1)
            (else cur))))
  ;next state of row n
  (define (row n out)
    (let ((w (length (car universe))))
      (if (= (length out) w)
          out
          (row n
               (cons (next-cell (- w (length out)) n)
                     out)))))
  ;a range of ints from bot to top
  (define (int-range bot top)
    (if (> bot top) '()
        (cons bot (int-range (+ bot 1) top))))
  (map (lambda (n)
         (row n '()))
       (int-range 1 (length universe))))

;represent the universe as a string
(define (universe->string universe)
  (define (prettify row)
    (apply string-append
           (map (lambda (b)
                  (if (= b 1) "#" "-"))
                row)))
  (if (null? universe)
      ""
      (string-append (prettify (car universe))
                     "\n"
                     (universe->string (cdr universe)))))

;starting with seed, show reps states of the universe
(define (conway seed reps)
  (when (> reps 0)
    (display (universe->string seed))
    (newline)
    (conway (next-universe seed) (- reps 1))))

;; --- Example Universes --- ;;

;blinker in a 3x3 universe
(conway '((0 1 0)
          (0 1 0)
          (0 1 0)) 5)

;glider in an 8x8 universe
(conway '((0 0 1 0 0 0 0 0)
          (0 0 0 1 0 0 0 0)
          (0 1 1 1 0 0 0 0)
          (0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0)
          (0 0 0 0 0 0 0 0)) 30)
