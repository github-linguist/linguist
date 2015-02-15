#lang racket
(require 2htdp/image 2htdp/universe)

;;;
;;; Grid object
;;;

(define (make-empty-grid m n)
  (build-vector m (lambda (y) (make-vector n 0))))

(define rows vector-length)

(define (cols grid)
  (vector-length (vector-ref grid 0)))

(define (make-grid m n living-cells)
  (let loop ([grid (make-empty-grid m n)]
             [cells living-cells])
    (if (empty? cells)
        grid
        (loop (2d-set! grid (caar cells) (cadar cells) 1) (cdr cells)))))

(define (2d-ref grid i j)
  (cond [(< i 0) 0]
        [(< j 0) 0]
        [(>= i (rows grid)) 0]
        [(>= j (cols grid)) 0]
        [else (vector-ref (vector-ref grid i) j)]))

(define (2d-refs grid indices)
  (map (lambda (ind) (2d-ref grid (car ind) (cadr ind))) indices))

(define (2d-set! grid i j val)
  (vector-set! (vector-ref grid i) j val)
  grid)

;;; cartesian product of 2 lists
(define (cart l1 l2)
  (if (empty? l1)
      '()
      (append (let loop ([n (car l1)] [l l2])
                (if (empty? l) '() (cons (list n (car l)) (loop n (cdr l)))))
              (cart (cdr l1) l2))))

;;; Count living cells in the neighbourhood
(define (n-count grid i j)
  (- (apply + (2d-refs grid (cart (list (- i 1) i (+ i 1))
                                  (list (- j 1) j (+ j 1)))))
     (2d-ref grid i j)))

;;;
;;; Rules and updates of the grid
;;;

;;; rules are stored in a 2d array: r_i,j = new state of a cell
;;; in state i with j neighboors
(define conway-rules
  (list->vector (list (list->vector '(0 0 0 1 0 0 0 0 0))
                      (list->vector '(0 0 1 1 0 0 0 0 0)))))

(define (next-state rules grid i j)
  (let ([current (2d-ref grid i j)]
        [N (n-count grid i j)])
    (2d-ref rules current N)))

(define (next-grid rules grid)
  (let ([new-grid (make-empty-grid (rows grid) (cols grid))])
    (let loop ([i 0] [j 0])
      (if (>= i (rows grid))
          new-grid
          (if (>= j (cols grid))
              (loop (+ i 1) 0)
              (begin (2d-set! new-grid i j (next-state rules grid i j))
                     (loop i (+ j 1))))))))

(define (next-grid! rules grid)
  (let ([new-grid (next-grid rules grid)])
    (let loop ((i 0))
      (if (< i (rows grid))
        (begin (vector-set! grid i (vector-ref new-grid i))
               (loop (+ i 1)))
        grid))))

;;;
;;; Image / Animation
;;;

(define (grid->image grid)
  (let ([m (rows grid)] [n (cols grid)] [size 5])
    (let loop ([img (rectangle (* m size) (* n size) "solid" "white")]
               [i 0] [j 0])
      (if (>= i (rows grid))
          img
          (if (>= j (cols grid))
              (loop img (+ i 1) 0)
              (if (= (2d-ref grid i j) 1)
                  (loop (underlay/xy img (* i (+ 1 size)) (* j (+ 1 size))
                                     (square (- size 2) "solid" "black"))
                        i (+ j 1))
                  (loop img i (+ j 1))))))))



(define (game-of-life grid refresh_time)
  (animate (lambda (n)
             (if (= (modulo n refresh_time) 0)
                 (grid->image (next-grid! conway-rules grid))
                 (grid->image grid)))))

;;;
;;; Examples
;;;

(define (blinker)
  (make-grid 3 3 '((0 1) (1 1) (2 1))))

(define (thunder)
  (make-grid 70 50 '((30 19) (30 20) (30 21) (29 17) (30 17) (31 17))))

(define (cross)
  (let loop ([i 0] [l '()])
    (if (>= i 80)
        (make-grid 80 80 l)
        (loop (+ i 1) (cons (list i i) (cons (list (- 79 i) i) l))))))

;;; To run examples:
;;; (game-of-life (blinker) 30)
;;; (game-of-life (thunder) 2)
;;; (game-of-life (cross) 2)
