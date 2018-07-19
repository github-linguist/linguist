#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require racket/fixnum)

; see the forest fire task, from which this is derived...
(define-struct wire-world (width height cells) #:prefab)

(define state:_ 0)
(define state:. 1)
(define state:H 2)
(define state:t 3)

(define (char->state c)
  (case c
    ((#\_ #\space) state:_)
    ((#\.) state:.)
    ((#\H) state:H)
    ((#\t) state:t)))

(define (initial-world l)
  (let ((h (length l))
        (w (string-length (first l))))
    (make-wire-world w h
                     (for*/fxvector
                      #:length (* h w)
                      ((row (in-list l))
                       (cell (in-string row)))
                      (char->state cell)))))

(define initial-list
  '("tH........."
    ".   .      "
    "   ...     "
    ".   .      "
    "Ht.. ......"))

(define-syntax-rule (count-neighbours-in-state ww wh wc r# c# state-to-match)
  (for/sum
      ((r (in-range (- r# 1) (+ r# 2)))
       #:when (< -1 r wh)
       (c (in-range (- c# 1) (+ c# 2)))
       #:when (< -1 c ww)
       ;; note, this will check cell at (r#, c#), too but it's not
       ;; worth checking that r=r# and c=c# each time in
       ;; this case, we know that (r#, c#) is a conductor:
       ; #:unless (and (= r# r) (= c# c))
       (i (in-value (+ (* r ww) c)))
       #:when (= state-to-match (fxvector-ref wc i)))
    1))

(define (cell-new-state ww wh wc row col)
  (let ((cell (fxvector-ref wc (+ col (* row ww)))))
    (cond
      ((= cell state:_) cell) ; empty -> empty
      ((= cell state:t) state:.) ; tail -> empty
      ((= cell state:H) state:t) ; head -> tail
      ((<= 1 (count-neighbours-in-state ww wh wc row col state:H) 2) state:H)
      (else cell))))

(define (wire-world-tick world)
  (define ww (wire-world-width world))
  (define wh (wire-world-height world))
  (define wc (wire-world-cells world))

  (define (/w x) (quotient x ww))
  (define (%w x) (remainder x ww))

  (make-wire-world
   ww wh
  (for/fxvector
   #:length (* ww wh)
   ((cell (in-fxvector wc))
    (r# (sequence-map /w (in-naturals)))
    (c# (sequence-map %w (in-naturals))))
   (cell-new-state ww wh wc r# c#))))

(define colour:_ (make-color   0   0   0))  ; black
(define colour:. (make-color  128 128 128)) ; grey
(define colour:H (make-color  128 255 255)) ; bright cyan
(define colour:t (make-color    0 128 128)) ; dark cyan

(define colour-vector (vector colour:_ colour:. colour:H colour:t))
(define (cell-state->colour state) (vector-ref colour-vector state))

(define render-scaling 20)
(define (render-world W)
  (define ww (wire-world-width W))
  (define wh (wire-world-height W))
  (define wc (wire-world-cells W))
   (let* ((flat-state
           (for/list ((cell (in-fxvector wc)))
             (cell-state->colour cell))))
     (place-image (scale render-scaling (color-list->bitmap flat-state ww wh))
                  (* ww (/ render-scaling 2))
                  (* wh (/ render-scaling 2))
                  (empty-scene (* render-scaling ww) (* render-scaling wh)))))

(define (run-wire-world #:initial-state W)
  (big-bang
   (initial-world W) ;; initial state
   [on-tick wire-world-tick
            1/8 ; tick time (seconds)
            ]
   [to-draw render-world]))

(run-wire-world #:initial-state initial-list)
