#lang racket
(require 2htdp/image)

;; The unsafe fixnum ops are faster than the checked ones,
;; but if you get anything wrong with them, they'll bite.
;; If you experience any problems reactivate the
;; (require racket/fixnum) and instead of the unsafe requirement
;; below...

;; we have tested this...
#;(require racket/fixnum)
;; so we can use this...
(require racket/require
           (only-in racket/fixnum make-fxvector in-fxvector)
           (filtered-in
            (? (name) (regexp-replace #rx"unsafe-" name ""))
            racket/unsafe/ops))

;;; This implementation uses a 1d, mutable, fixnum vector
;;; there's a lot of work done making the tree, so this optimisation
;;; at the expense of clarity has been made. Sorry, guys!
(define (brownian-tree w h collisions n-particles seed-tree
                       generate-particle walk-particle)
  (define w*h (fx* w h))
  (define V (make-fxvector w*h))
  (define (collision? x.y) (fx> (fxvector-ref V x.y) 0))

  ;; The main loop
  (define (inner-b-t collisions particles)
    (cond
      [(fx= 0 collisions) V]
      [else
       (define-values (new-particles new-collisions)
         (for/fold
             ((prtcls null)
              (clsns 0))
           ((x.y particles)
            #:break (fx= collisions clsns))
           (define new-particle (walk-particle x.y w h w*h))
           (cond
             [(not new-particle) ; it died!
              (values (cons (generate-particle V w h w*h) prtcls) clsns)]
             [(collision? new-particle)
              (fxvector-set! V x.y 1)
              (values (cons (generate-particle V w h w*h) prtcls) (add1 clsns))]
             [else
              (values (cons new-particle prtcls) clsns)])))
       (when (fx> new-collisions 0)
         (define remain (fx- collisions new-collisions))
         (unless (fx= (exact-floor (* 10 (log collisions)))
                      (exact-floor (* 10 (log (fxmax 1 remain)))))
           (eprintf "~a (e^~a)~%" remain (log (fxmax 1 remain))))
         (log-info "~a new collisions: ~a remain~%" new-collisions remain))
       (inner-b-t (fxmax 0 (fx- collisions new-collisions)) new-particles)]))

  ;; Seed the tree
  (seed-tree V w h)
  (inner-b-t collisions
             (build-list n-particles
                         (lambda (x) (generate-particle V w h w*h)))))

;; See below for why we do the (fxremainder ...) test
(define (uniform-particle-generator v w h w*h)
  (define x.y (random w*h))
  (define valid-x.y?
    (and
     (fx= (fxvector-ref v x.y) 0) ; start on empty cell
     (fx> (fxremainder x.y w) 0))) ; not on left edge
  ; if it's valid take it otherwise regenerate
  (if valid-x.y? x.y (uniform-particle-generator v w h w*h)))

;; The boundaries to the walker are to remain within the limits of
;; the vector... however, unless we stop particles going off the
;; east/west edges, the tree will be formed on a cylinder as the
;; particles wrap. So we kill particles that reach the left edge
;; either by decrement from the right or by incrementing and wrapping.
;; This is is tested with (= 0 (remainder x.y w)).
(define (brownian-particle-walker x.y w h w*h)
  (define dx (fx- (random 3) 1))
  (define dy (fx- (random 3) 1))
  (define new-x.y (fx+ x.y (fx+ dx (fx* w dy))))
  (and (fx> new-x.y 0) (fx< new-x.y w*h)
       (fx> (fxremainder new-x.y w) 0)
       new-x.y))

;; These seed functions modify v however you want!
(define (seed-middle v w h)
  (fxvector-set! v (+ (quotient w 2) (* w (quotient h 2))) 1))

(define (seed-circle v w h)
  (for ((a (in-range 0 360 120)))
    (define x (exact-floor (* w 1/8 (+ 4 (sin (* pi 1/180 a))))))
    (define y (exact-floor (* h 1/8 (+ 4 (cos (* pi 1/180 a))))))
    (fxvector-set! v (+ x (* w y)) 1)))

;; SCALE is a general purpose knob for modifying the size of the problem
;;       complexity increases with the sqaure of SCALE (at least)
(define SCALE 1)
(define tree-W (* SCALE 320))
(define tree-H (* SCALE 240))
(define tree-W.H (* tree-W tree-H))
;; play with tree-PARTICLES -- small values will lead to a smaller tree
;; as the tree moves towards the edges, more particles might affect its shape
(define tree-PARTICLES (quotient tree-W.H 4))
;; these are the particles that are bimbling around at any one time. If it's
;; too low, you might get bored waiting for a collision... if it's too high
;; you might get inappropriate collisions
(define working-PARTICLES (quotient tree-W.H 300))

(define b-t (time
             (brownian-tree
              tree-W tree-H tree-PARTICLES working-PARTICLES
              seed-middle
              uniform-particle-generator
              brownian-particle-walker)))

(define (b-t-value->color c) (case c ((1) "black") (else "white")))
(define img (color-list->bitmap
             (for*/list ((x (in-fxvector b-t)))
               (b-t-value->color x))
             tree-W tree-H))

img
(save-image img "brownian-tree.png")
