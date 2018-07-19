#lang racket

(struct Hex (x y letter clicked?) #:mutable #:transparent)

(define hexes
  (let* ([A (char->integer #\A)]
         [letters (take (shuffle (map (compose string integer->char)
                                      (range A (+ A 26))))
                        20)])
    (for*/list ([row 4] [column 5])
      (Hex (* 3/2 column) (* 2 (+ row (if (odd? column) 1/2 0)))
           (list-ref letters (+ (* 5 row) column))
           false))))

(require 2htdp/image)
(define (blank width height) (rectangle width height 'outline (color 0 0 0 0)))

(define (hexagon mode color) (regular-polygon 1 6 mode color))
(define aspect-ratio (sin (/ pi 3)))

(define (board _)
  (scale 100
         (for/fold ([the-board (blank 8 (* aspect-ratio 9))])
           ([hex hexes])
           (define-values (letter-color hex-color)
             (if (Hex-clicked? hex) (values 'black 'purple) (values 'red 'yellow)))
           (underlay/align/offset
            'left 'top the-board
            (Hex-x hex) (* aspect-ratio (Hex-y hex))
            (overlay (scale 1/10 (text (Hex-letter hex) 10 letter-color))
                     (hexagon 'outline 'black)
                     (hexagon 'solid hex-color))))))

#| Closest hex in hexes to x y, as one with minimum distance to its center. |#
(define (hex-at x y)
  (argmin (λ (hex) (+ (sqr (- x (* 100 (add1 (Hex-x hex)))))
                      (sqr (- y (* aspect-ratio 100 (add1 (Hex-y hex)))))))
          hexes))

(define letters-chosen '())
(define (choose hex)
  (set-Hex-clicked?! hex true)
  (define letter (Hex-letter hex))
  (when (not (member letter letters-chosen))
    (set! letters-chosen (list* (Hex-letter hex) letters-chosen))))

(require 2htdp/universe)
(void (big-bang
       (void)
       [to-draw board]
       [stop-when (λ (_) (andmap Hex-clicked? hexes)) board]
       [on-key (λ (_ k)
                 (define hex (findf (λ (hex) (key=? k (string-downcase (Hex-letter hex))))
                                    hexes))
                 (when hex (choose hex)))]
       [on-mouse (λ (_ x y event-type)
                   (when (equal? "button-down" event-type)
                     (choose (hex-at x y))))]))

(displayln "The letters were chosen in the order:")
(for-each display (add-between (reverse letters-chosen) " "))
