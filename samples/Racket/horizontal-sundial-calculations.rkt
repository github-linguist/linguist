#lang racket

;; print the table for a given latitude and longitude-offset,
;; given in degrees
(define (print-table lat long-offset)
  ;; print the table header
  (display
   (~a "    sine of latitude: "
       (~r (sin (deg->rad lat)) #:precision '(= 3))
       "\n"
       "    diff longitude:   "
       (~r long-offset #:precision '(= 3))
       "\n\nHour, sun hour angle, dial hour line angle "
       "from 6am to 6pm\n"))
  ;; print the table
  (for ([h (in-range -6 7)])
    (define hra (- (* 15 h) long-offset))
    (define hla (to-hla lat hra))
    (display (~a "HR="(pad-to 3 (~a h))"; "
                 "HRA="(pad-to 7 (~r hra #:precision '(= 3)))"; "
                 "HLA="(pad-to 7 (~r hla #:precision '(= 3)))"\n"))))


;; compute the angle on the gnomon corresponding to a
;; given angle of the sun (angles given and returned in degrees)
(define (to-hla lat ang)
  (define lat-sign (cond [(< lat 0) -1] [else 1]))
  ;; move to the right quadrant for
  ;; angles outside [-90,90]
  (define correction (* (cond [(< ang -90) -180]
                              [(> ang 90) 180]
                              [else 0])
                        lat-sign))
  (+ (rad->deg (atan (* (sin (deg->rad lat))
                        (tan (deg->rad ang)))))
     correction))

;; write the prompt, return the entered number
(define (prompt->num p)
  (printf "~a" p)
  (string->number (read-line)))

;; translate degrees to radians
(define (deg->rad d) (* 2 pi (/ d 360)))

;; translate radians to degrees
(define (rad->deg r) (* 360 (/ r (* 2 pi))))

;; add spaces to reach given length
(define (pad-to cols str)
  (define spaces-needed (max 0 (- cols (string-length str))))
  (string-append
   (list->string (for/list ([i spaces-needed]) #\space))
   str))


;; INPUT PARAMETERS, PRINT TABLE:
(define lat (prompt->num "Enter latitude       => "))
(define lng (prompt->num "Enter longitude      => "))
(define ref (prompt->num "Enter legal meridian => "))

(print-table lat (- lng ref))

;; test cases for angle conversion
(require rackunit)
(check < (to-hla 30 89) 90)
(check-= (to-hla 30 90) 90 1e-5)
(check > (to-hla 30 91) 90)
(check > (to-hla 30 -89) -90)
(check-= (to-hla 30 90) 90 1e-5)
(check < (to-hla 30 -91) -90)
(check < (to-hla -30 -89) 90)
(check-= (to-hla -30 -90) 90 1e-5)
(check > (to-hla -30 -91) 90)
(check > (to-hla -30 89) -90)
(check-= (to-hla -30 90) -90 1e-5)
(check < (to-hla -30 91) -90)
