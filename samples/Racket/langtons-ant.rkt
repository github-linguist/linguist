#lang racket

;; contracts allow us to describe expected behaviour of funcitons
(define direction/c (or/c 'u 'r 'l 'd))
(define turn/c (-> direction/c direction/c))
(define grid/c (hash/c integer? (hash/c integer? boolean?)))
(define-struct/contract ant ([d direction/c] [x integer?] [y integer?]))

(define/contract (turn-right dir) turn/c
  (case dir ((u) 'r) ((d) 'l) ((r) 'd) ((l) 'u)))

(define/contract (turn-left dir) turn/c
  (case dir ((u) 'l) ((d) 'r) ((r) 'u) ((l) 'd)))

(define/contract (move d x y)
  (-> direction/c integer? integer? (list/c direction/c integer? integer?))
  (list
   d
   (+ x (case d ((l) -1) ((r) 1) (else 0)))
   (+ y (case d ((u) -1) ((d) 1) (else 0)))))


(define/contract (move-ant d a) (-> direction/c ant? ant?)
  (apply make-ant (move d (ant-x a) (ant-y a))))

(define/contract (langton a grid) (-> ant? grid/c grid/c)
  (let ((ax (ant-x a)) (ay (ant-y a)))
    (if (and (<= 1 ax 100) (<= 1 ay 100))
        (let* ((grid-row (hash-ref grid ay hash))
               (cell-black? (hash-ref grid-row ax #f)))
          (langton
           (move-ant ((if cell-black? turn-left turn-right) (ant-d a)) a)
           (hash-set grid ay (hash-set grid-row ax (not cell-black?)))))
        grid)))

(define/contract (show-grid/text grid) (-> grid/c void?)
  (for* ; for* allows us to refer to y in rw
      ((y (in-range 1 101))
       (rw (in-value (hash-ref grid y #f)))
       #:when rw        ; if there is no row, the ant never visisted it
       #:when (newline) ; when can be used simply for its side effect
       (x (in-range 1 101)))
    (case (hash-ref rw x #\?)
      ((#\?) (display #\space)) ; distingush between "ant-visited white" vs. pure white
      ((#f)  (display #\:))     ; little anty footprints left
      ((#t)  (display #\#)))))


(show-grid/text (langton (make-ant 'u 50 50) (hash)))

(require 2htdp/image)
(define/contract (show-grid/png grid) (-> grid/c image?)
  (for*/fold
      ((scn (empty-scene 408 408)))
       ((y (in-range 1 101))
       (rw (in-value (hash-ref grid y #f)))
       #:when rw        ; if there is no row, the ant never visisted it
       (x (in-range 1 101)))
    (case (hash-ref rw x #\?)
      ((#\?) scn) ; distingush between "ant-visited white" vs. pure white
      ((#f)  (place-image (circle 2 "outline" "gray") (* x 4) (* y 4) scn))     ; little anty footprints left
      ((#t)  (place-image (circle 2 "solid" "black")  (* x 4) (* y 4) scn)))))
(show-grid/png (langton (make-ant 'u 50 50) (hash)))
