(import (scheme base)
        (scheme read)
        (scheme write)
        (srfi 18)
        )

(thread-start!
  (make-thread
    (lambda ()
      (thread-sleep! 1.2)
      (display "started thread, this should be written to console")
      (newline)
      (display "thread done")
      (newline)
      (flush-output-port (current-output-port)))))

(thread-sleep! 1) ;; Prevent race condition replacing stdout before thread is spawned
(write `(1 2 3))
(define fp (open-output-file "tmp.txt"))
(parameterize
  ((current-output-port fp))
  (write `(4 5 6))
  (thread-sleep! 3)
)
(close-port fp)
(write `(7 8 9))

;;; The game of life example from r7rs.
;;; Main program

(import (scheme base)
        (only (example life) life)
        (rename (prefix (example grid) grid-)
                (grid-make make-grid)))
;; Initialize a grid with a glider.
(define grid (make-grid 24 24))
(grid-put! grid 1 1 #t)
(grid-put! grid 2 2 #t)
(grid-put! grid 3 0 #t)
(grid-put! grid 3 1 #t)
(grid-put! grid 3 2 #t)
;; Run for x iterations.
(life grid 80)


(define-library (example life)
  (import (except (scheme base) set!))
  (cond-expand
    (cyclone
     (import (scheme write))
     (import (example grid))

     ))

  (cond-expand
    (cyclone
  (export life)
  ))

  (cond-expand
    (cyclone
    (begin
    (define (life-count grid i j)
      (define (count i j)
        (if (ref grid i j) 1 0))
      (+ (count (- i 1) (- j 1))
         (count (- i 1) j)
         (count (- i 1) (+ j 1))
         (count i (- j 1))
         (count i (+ j 1))
         (count (+ i 1) (- j 1))
         (count (+ i 1) j)
         (count (+ i 1) (+ j 1))))
    )
    (begin
    (define (life-alive? grid i j)
      (case (life-count grid i j)
            ((3) #t)
            ((2) (ref grid i j))
            (else #f)))
)

    )
  )
  (begin
    (define (clear-vt100)
        (display
            (string
                (integer->char #x1B)
                #\[
                #\H
                (integer->char #x1B)
                #\[
                #\J)))
    (define (life-print grid)
      (clear-vt100)
      (each grid
        (lambda (i j v)
          (display (if v "*" " "))
          (if (= j (- (cols grid) 1))
          ;(when (= j (- (cols grid) 1))
            (newline)))))
    (define (life grid iterations)
      (do ((i 0 (+ i 1))
           (grid0 grid grid1)
           (grid1 (make (rows grid) (cols grid))
                  grid0))
          ((= i iterations))
        (each grid0
          (lambda (j k v)
            (let ((a (life-alive? grid0 j k)))
              (put! grid1 j k a))))
              ;(set! grid1 j k a))))
        (life-print grid1)))))
