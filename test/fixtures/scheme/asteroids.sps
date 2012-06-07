(import (rnrs)
        (only (surfage s1 lists) filter-map)
        (gl)
        (glut)
        (dharmalab records define-record-type)
        (dharmalab math basic)
        (agave glu compat)
        (agave geometry pt)
        (agave glamour window)
        (agave glamour misc)
        (surfage s19 time)
        (surfage s27 random-bits)
        (surfage s42 eager-comprehensions))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (say . args)
  (for-each display args)
  (newline))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gl-translate-pt p)
  (glTranslated (pt-x p) (pt-y p) 0.0))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (radians x) (* x (/ pi 180)))

(define (degrees x) (* x (/ 180 pi)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (angle->pt a)
  (pt (cos a)
      (sin a)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (current-time-in-nanoseconds)
  (let ((val (current-time)))
    (+ (* (time-second val) 1000000000)
       (time-nanosecond val))))

(define (current-time-in-seconds)
  (/ (current-time-in-nanoseconds)
     1000.0 ;; micro
     1000.0 ;; milli
     1000.0))

(define base-time (current-time-in-seconds))

(define (time-step) (- (current-time-in-seconds) base-time))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define score 0)

(define level 1)

(define ships 3)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spaceship
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type++ spaceship
  (fields (mutable pos)
          (mutable vel)
          (mutable theta)
          (mutable force)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; particle
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type++ particle
  (fields (mutable pos)
          (mutable vel)
          (mutable birth)
          (mutable lifetime)
          (mutable color)))

(define particles '())

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bullet
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type++ bullet
  (fields (mutable pos)
          (mutable vel)
          (mutable birth)))

(define bullets '())

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; asteroid
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type++ asteroid
  (fields (mutable pos)
          (mutable vel)
          (mutable radius)))

(define number-of-starting-asteroids 4)

(define asteroids #f)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bullet-pack
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type++ bullet-pack
  (fields (mutable pos)
          (mutable vel)))

(define pack #f)

(is-bullet-pack pack)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(initialize-glut)

(window (size 800 400)
        (title "Asteroids")
        (reshape (width height)))

(random-source-randomize! default-random-source)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pt-wrap p)
  (pt (mod (pt-x p) width)
      (mod (pt-y p) height)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ship
  (make-spaceship (pt (/ width 2.0) (/ height 2.0))
                  (pt 0.0 0.0)
                  0.0
                  0.0))

(is-spaceship ship)

(define ammo 0)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set! asteroids
      (list-ec (: i number-of-starting-asteroids)
        (make-asteroid (pt (inexact (random-integer width))
                           (inexact (random-integer height)))
                       (pt (inexact (+ -50 (random-integer 100)))
                           (inexact (+ -50 (random-integer 100))))
                       50.0)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set! pack (make-bullet-pack (pt (inexact (random-integer width))
                                 (inexact (random-integer height)))
                             (pt (inexact (+ -50 (random-integer 100)))
                                 (inexact (+ -50 (random-integer 100))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(buffered-display-procedure
 (lambda ()
   (background 0.0)

   ;; ship

   (glColor3f 0.0 1.0 0.0)

   (gl-matrix-excursion
    (gl-translate-pt ship.pos)
    (glRotated 90.0 0.0 1.0 0.0)
    (glRotated (degrees ship.theta) -1.0 0.0 0.0)
    (glutWireCone 10.0 30.0 5 5))

   ;; particles

   (for-each
    (lambda (par)

      (let ((c (particle-color par)))
        (glColor3f (vector-ref c 0)
                   (vector-ref c 1)
                   (vector-ref c 2)))
      
      (gl-matrix-excursion
       (gl-translate-pt (particle-pos par))
       (glutWireSphere 2.0 5 5)))
    particles)

   ;; bullets

   (glColor3f 0.0 0.0 1.0)
   
   (for-each
    (lambda (bullet)
      (gl-matrix-excursion
       (gl-translate-pt (bullet-pos bullet))
       (glutWireSphere 5.0 10 10)))
    bullets)

   ;; asteroids

   (glColor3f 1.0 0.0 0.0)

   (for-each
    (lambda (asteroid)
      (gl-matrix-excursion
       (gl-translate-pt (asteroid-pos asteroid))
       (glutWireSphere (asteroid-radius asteroid) 10 10)))
    asteroids)

   ;; bullet-pack

   (glColor3f 0.0 0.0 1.0)

   (gl-matrix-excursion
    (gl-translate-pt pack.pos)
    (glutWireCube 10.0))

   ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define last-time (current-time-in-seconds))

(define dt 0)

(define (update-system)

  (set! dt (- (current-time-in-seconds) last-time))

  (set! last-time (current-time-in-seconds))

  (ship.pos! (pt-wrap (pt+ ship.pos (pt*n ship.vel dt))))

  (pack.pos! (pt-wrap (pt+ pack.pos (pt*n pack.vel dt))))

  (set! particles
        (filter-map
         (lambda (par)
           (is-particle par)
           (cond ((> (- (current-time-in-seconds) par.birth) par.lifetime) #f)
                 (else (par.pos! (pt+ par.pos (pt*n par.vel dt)))
                       par)))
         particles))

  (set! bullets
        (filter-map
         (lambda (bullet)
           (is-bullet bullet)
           (cond ((> (- (current-time-in-seconds) bullet.birth) 2.0) #f)
                 (else (bullet.pos! (pt+ bullet.pos (pt*n bullet.vel dt)))
                       bullet)))
         bullets))

  (set! asteroids
        (filter-map
         (lambda (a)
           (is-asteroid a)
           (a.pos! (pt-wrap (pt+ a.pos (pt*n a.vel dt))))
           (if (< a.radius 10.0) #f a))
         asteroids))

  ;; bullet asteroid contact

  (for-each
   (lambda (b)
     (is-bullet b)
     (for-each
      (lambda (a)
        (is-asteroid a)
        (when (<= (pt-distance b.pos a.pos)
                  a.radius)
          
          (begin (set! score (+ score 1))
                 (say "score: " score)
                 #f)
          
          (set! asteroids
                (append 
                 (list-ec (: i 4)
                   (make-asteroid a.pos
                                  (pt (+ -50.0 (random-integer 100))
                                      (+ -50.0 (random-integer 100)))
                                  (/ a.radius 2.0)))
                 asteroids))
          (a.radius! 0.1)
          (b.birth! 0.0)

          (set! particles
                (append (list-ec (: i 100)
                          (make-particle a.pos
                                         (pt*n (angle->pt
                                                (radians
                                                 (random-integer 360)))

                                               (random-integer 100)

                                               )
                                         (current-time-in-seconds)
                                         1.0
                                         (vector 1.0 1.0 1.0)))
                        particles))))
      asteroids))
   bullets)

  (for-each
   (lambda (a)
     (is-asteroid a)
     (when (<= (pt-distance a.pos ship.pos) a.radius)

       (set! particles
             (append (list-ec (: i 100)
                       (make-particle ship.pos
                                      (pt*n (angle->pt
                                             (radians
                                              (random-integer 360)))
                                            (random-integer 100))
                                      (current-time-in-seconds)
                                      1.0
                                      (vector 0.0 1.0 1.0)))
                     particles))

       (set! ship (make-spaceship (pt (/ width 2.0) (/ height 2.0))
                                  (pt 0.0 0.0)
                                  0.0
                                  0.0))

       ))
   asteroids)

  (when (null? asteroids)
    (set! level (+ level 1))
    (display "level: ")
    (display level)
    (newline)
    (set! asteroids
          (list-ec (: i (+ number-of-starting-asteroids level))
            (make-asteroid (pt (inexact (random-integer width))
                               (inexact (random-integer height)))
                           (pt (inexact (+ -50 (random-integer 100)))
                               (inexact (+ -50 (random-integer 100))))
                           50.0))))

  ;; ship pack contact

  (when (<= (pt-distance ship.pos pack.pos) 10.0)
    (set! ammo (+ ammo 5))
    (set! pack (make-bullet-pack (pt (inexact (random-integer width))
                                     (inexact (random-integer height)))
                                 (pt (inexact (+ -50 (random-integer 100)))
                                     (inexact (+ -50 (random-integer 100))))))
    (say "ammo: " ammo))

  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(glutIdleFunc
 (lambda ()
   (update-system)
   (glutPostRedisplay)))

(glutKeyboardFunc
 (lambda (key x y)
   (case (integer->char key)

     ((#\w)

      (ship.vel! (pt+ ship.vel (pt*n (angle->pt ship.theta) 50.0)))

      (set! particles
            (append (list-ec (: i 10)
                      (make-particle ship.pos
                                     (pt*n
                                      (angle->pt
                                       (+ ship.theta
                                          (radians 180.0)
                                          (radians (+ -45 (random-integer 90)))
                                          ))
                                      (random-integer 50)
                                      )
                                     (current-time-in-seconds)
                                     1.0
                                     (vector 1.0 1.0 0.0)))
                    particles))

      )

     ((#\a) (ship.theta! (+ ship.theta (radians 20.0))))
     ((#\d) (ship.theta! (- ship.theta (radians 20.0))))

     ((#\s) (ship.vel! (pt 0.0 0.0)))

     ((#\x) (ship.theta! (+ ship.theta (radians 180.0))))

     ((#\space)

      (when (> ammo 0)

        (set! ammo (- ammo 1))
      
        (set! bullets
              (cons
               (make-bullet ship.pos
                            (pt+ ship.vel
                                 (pt*n (angle->pt ship.theta) 400.0))
                            (current-time-in-seconds))
               bullets)))

      (say "ammo: " ammo)
      )
     )))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(say "w   - Thrusters")
(say "a/d - Left/Right")
(say "s   - Stop")
(say "x   - Flip")
(say "spc - Laser")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(glutMainLoop)
