#lang racket
(require 2htdp/universe)
(require 2htdp/image)

(define (initial-forest w p-tree)
  (for/vector #:length w ((rw w))
    (for/vector #:length w ((cl w))
      (if (< (random) p-tree) #\T #\_))))

(define (has-burning-neighbour? forest r# c# w)
  ;; note, this will check r# c#, too but it's not
  ;; worth checking that r=r# and c=c# each time in
  ;; this case
  (for*/first
      ((r (in-range (- r# 1) (+ r# 2)))
       #:when (< 0 r w)
       (c (in-range (- c# 1) (+ c# 2)))
       #:when (< 0 c w)
       #:when (equal? #\* (vector-ref (vector-ref forest r) c)))
    #t))

(define (fire-tick forest p-sprout f-combust w)
  (for/vector #:length w ((rw forest) (r# (in-naturals)))
    (for/vector #:length w ((cl rw) (c# (in-naturals)))
      (case cl
        ((#\_) (if (< (random) p-sprout) #\T #\_))
        ((#\*) #\_)
        ((#\T)
         (cond
           [(has-burning-neighbour? forest r# c# w) #\*]
           [(< (random) f-combust) #\*]
           [else #\T]))))))

(define (render-forest state)
  (for/fold
      ((scn (empty-scene
             (* (vector-length state) 8)
             (* (vector-length (vector-ref state 0)) 8)
             'black)))

    ((rw state) (r# (in-naturals)))
    (for/fold
        ((scn scn))
      ((cl rw) (c# (in-naturals)))
      (place-image (circle 4 'solid
                           (case cl
                             ((#\_) 'brown)
                             ((#\T) 'green)
                             ((#\*) 'red)))
                   (+ 4 (* c# 8)) (+ 4 (* r# 8)) scn))))

(define (forest-fire p-tree p-sprout f-combust w)
  (big-bang
   (initial-forest w p-tree) ;; initial state
   [on-tick (lambda (state)
              ;(displayln state)
              (fire-tick state p-sprout f-combust w))]
   [to-draw render-forest]))

(forest-fire 0 1/8 1/1024 50)
