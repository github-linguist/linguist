;a ball's position...row is a natural number and col is an integer where 0 is the center
(define-struct pos (row col))
;state of simulation...list of all positions and vector of balls (index = bin)
(define-struct st (poss bins))
;increment vector @i
(define (vector-inc! v i) (vector-set! v i (add1 (vector-ref v i))))

(define BALL-RADIUS 6)
;for balls to fit perfectly between diamond-shaped pins, the side length is
;determined by inscribing the diamond in the circle
(define PIN-SIDE-LENGTH (* (sqrt 2) BALL-RADIUS))
;ultimate pin width and height
(define PIN-WH (* 2 BALL-RADIUS))
(define PIN-HOR-SPACING (* 2 PIN-WH))
;vertical space is the height of an equilateral triangle with side length = PIN-HOR-SPACING
(define PIN-VER-SPACING (* 1/2 (sqrt 3) PIN-HOR-SPACING))
;somewhat copying BASIC256's graphics
;determines how thick the outline will be
(define FILL-RATIO 0.7)
;freeze is a function that converts the drawing code into an actual bitmap forever
(define PIN (freeze (overlay (rotate 45 (square (* FILL-RATIO PIN-SIDE-LENGTH) "solid" "purple"))
                             (rotate 45 (square PIN-SIDE-LENGTH "solid" "magenta")))))
(define BALL (freeze (overlay (circle (* FILL-RATIO BALL-RADIUS) "solid" "green")
                              (circle BALL-RADIUS "solid" "dark green"))))
(define BIN-COLOR (make-color 255 128 192))
;# balls bin can fit
(define BIN-CAPACITY 10)
(define BIN-HEIGHT (* BIN-CAPACITY PIN-WH))
(define BIN (freeze (beside/align "bottom"
                                  (line 0 BIN-HEIGHT BIN-COLOR)
                                  (line PIN-WH 0 BIN-COLOR)
                                  (line 0 BIN-HEIGHT BIN-COLOR))))

(define draw-background
  (let ([background #f])
    (λ (height)
      (if (image? background)
          background
          (let* ([w (+ (image-width BIN) (* PIN-HOR-SPACING height))]
                 [h (+ PIN-WH (image-height BIN) (* PIN-VER-SPACING height))]
                 [draw-background (λ () (rectangle w h "solid" "black"))])
            (begin (set! background (freeze (draw-background))) background))))))

;draws images using x horizontal space between center points
(define (spaced/x x is)
  (if (null? is)
      (empty-scene 0 0)
      (let spaced/x ([n 1] [i (car is)] [is (cdr is)])
        (if (null? is)
            i
            (overlay/xy i (* -1 n x) 0 (spaced/x (add1 n) (car is) (cdr is)))))))

(define (draw-pin-row r) (spaced/x PIN-HOR-SPACING (make-list (add1 r) PIN)))

;draws all pins, using saved bitmap for efficiency
(define draw-pins
  (let ([bmp #f])
    (λ (height)
      (let ([draw-pins (λ () (foldl (λ (r i) (overlay/align/offset
                                              ;vertically line up all pin rows
                                              "center" "bottom" (draw-pin-row r)
                                              ;shift down from the bottom of accum'ed image by ver spacing
                                              0 (- PIN-VER-SPACING) i))
                                    (draw-pin-row 0) (range 1 height 1)))])
        (if (image? bmp)
            bmp
            (begin (set! bmp (freeze (draw-pins))) bmp))))))

(define (draw-ball p i)
  ;the ball starts at the top of the image
  (overlay/align/offset "center" "top" BALL (* -1 (pos-col p) PIN-WH) (* -1 (pos-row p) PIN-VER-SPACING) i))

;bin has balls added from bottom, stacked exactly on top of each other
;the conditional logic is needed because above can't handle 0 or 1 things
(define (draw-bin n)
  (if (zero? n)
      BIN
      (overlay/align "center" "bottom"
                     (if (= n 1) BALL (apply above (make-list n BALL)))
                     BIN)))

;main drawing function
(define (draw height s)
  (let* ([bins (spaced/x PIN-HOR-SPACING (map draw-bin (vector->list (st-bins s))))]
         ;pins above bins
         [w/pins (above (draw-pins height) bins)]
         ;draw this all one ball diameter (PIN-WH) below top
         [w/background (overlay/align/offset "center" "top" w/pins
                                             0 (- PIN-WH) (draw-background height))])
    ;now accumulate in each ball
    (foldl draw-ball w/background (st-poss s))))

;a ball moves down by increasing its row and randomly changing its col by -1 or 1
(define (next-row height p)
  (make-pos (add1 (pos-row p))
            (+ -1 (* 2 (random 2)) (pos-col p))))

;each step, every ball goes to the next row and new balls are added at the top center
;balls that fall off go into bins
(define (tock height s)
  (let* ([new-ps (map (λ (p) (next-row height p)) (st-poss s))]
         ;live balls haven't gone past the last row of pins
         [live (filter (λ (p) (< (pos-row p) height)) new-ps)]
         ;dead balls have (partition from normal Racket would be useful here...)
         [dead (filter (λ (p) (>= (pos-row p) height)) new-ps)]
         ;adjust col from [-x,x] to [0,2x]
         [bin-indices (map (λ (p) (quotient (+ (pos-col p) height) 2)) dead)])
    ;add new balls to the live balls
    (make-st (append (make-list (random 4) (make-pos 0 0)) live)
             ;sum dead ball positions into bins
             (begin (for-each (λ (i) (vector-inc! (st-bins s) i)) bin-indices)
                    (st-bins s)))))

;run simulation with empty list of positions to start, stepping with "tock" and drawing with "draw"
(define (run height)
  (big-bang (make-st '() (make-vector (add1 height) 0))
            (on-tick (λ (ps) (tock height ps)) 0.5)
            (to-draw (λ (ps) (draw height ps)))))
