#!r6rs

;;; R6RS implementation of Pendulum Animation

(import (rnrs)
        (lib pstk main) ; change this for your pstk installation
        )

(define PI 3.14159)
(define *conv-radians* (/ PI 180))
(define *theta* 45.0)
(define *d-theta* 0.0)
(define *length* 150)
(define *home-x* 160)
(define *home-y* 25)

;;; estimates new angle of pendulum
(define (recompute-angle)
  (define (avg a b) (/ (+ a b) 2))
  (let* ((scaling (/ 3000.0 (* *length* *length*)))
         ; first estimate
         (first-dd-theta (- (* (sin (* *theta* *conv-radians*)) scaling)))
         (mid-d-theta (+ *d-theta* first-dd-theta))
         (mid-theta (+ *theta* (avg *d-theta* mid-d-theta)))
         ; second estimate
         (mid-dd-theta (- (* (sin (* mid-theta *conv-radians*)) scaling)))
         (mid-d-theta-2 (+ *d-theta* (avg first-dd-theta mid-dd-theta)))
         (mid-theta-2 (+ *theta* (avg *d-theta* mid-d-theta-2)))
         ; again first
         (mid-dd-theta-2 (- (* (sin (* mid-theta-2 *conv-radians*)) scaling)))
         (last-d-theta (+ mid-d-theta-2 mid-dd-theta-2))
         (last-theta (+ mid-theta-2 (avg mid-d-theta-2 last-d-theta)))
         ; again second
         (last-dd-theta (- (* (sin (* last-theta *conv-radians*)) scaling)))
         (last-d-theta-2 (+ mid-d-theta-2 (avg mid-dd-theta-2 last-dd-theta)))
         (last-theta-2 (+ mid-theta-2 (avg mid-d-theta-2 last-d-theta-2))))
    ; put values back in globals
    (set! *d-theta* last-d-theta-2)
    (set! *theta* last-theta-2)))

;;; The main event loop and graphics context
(let ((tk (tk-start)))
  (tk/wm 'title tk "Pendulum Animation")
  (let ((canvas (tk 'create-widget 'canvas)))

    ;;; redraw the pendulum on canvas
    ;;; - uses angle and length to compute new (x,y) position of bob
    (define (show-pendulum canvas)
      (let* ((pendulum-angle (* *conv-radians* *theta*))
             (x (+ *home-x* (* *length* (sin pendulum-angle))))
             (y (+ *home-y* (* *length* (cos pendulum-angle)))))
        (canvas 'coords 'rod *home-x* *home-y* x y)
        (canvas 'coords 'bob (- x 15) (- y 15) (+ x 15) (+ y 15))))

    ;;; move the pendulum and repeat after 20ms
    (define (animate)
      (recompute-angle)
      (show-pendulum canvas)
      (tk/after 20 animate))

    ;; layout the canvas
    (tk/grid canvas 'column: 0 'row: 0)
    (canvas 'create 'line 0 25 320 25 'tags: 'plate 'width: 2 'fill: 'grey50)
    (canvas 'create 'oval 155 20 165 30 'tags: 'pivot 'outline: "" 'fill: 'grey50)
    (canvas 'create 'line 1 1 1 1 'tags: 'rod 'width: 3 'fill: 'black)
    (canvas 'create 'oval 1 1 2 2 'tags: 'bob 'outline: 'black 'fill: 'yellow)

    ;; get everything started
    (show-pendulum canvas)
    (tk/after 500 animate)
    (tk-event-loop tk)))
