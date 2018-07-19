#lang racket

;;; Generate the headings and boxes
(define (i->heading/box i)
  (values (let ((heading (* i #e11.25)))
            (case (modulo i 3)
             ((1) (+ heading #e5.62))
             ((2) (- heading #e5.62))
             (else heading)))
   (add1 (modulo i 32))))
(define-values (headings-list box-list)
  (for/lists (h-lst i-lst) ((i (in-range 0 (add1 32))))
             (i->heading/box i)))

(define box-names
  (list "North" "North by east" "North-northeast"
        "Northeast by north" "Northeast" "Northeast by east"
        "East-northeast" "East by north" "East" "East by south" "East-southeast"
        "Southeast by east" "Southeast" "Southeast by south" "South-southeast"
        "South by east" "South" "South by west" "South-southwest"
        "Southwest by south" "Southwest" "Southwest by west"
        "West-southwest" "West by south" "West" "West by north" "West-northwest"
        "Northwest by west" "Northwest" "Northwest by north" "North-northwest"
        "North by west"))

(define (heading->box h)
 (let* ((n-boxes (length box-names))
        (box-width (/ 360 n-boxes)))
  (add1 (modulo (ceiling (- (/ h box-width) 1/2)) n-boxes))))

;;; displays a single row of the table, can also be used for titles
(define (display-row b a p)
  (printf "~a | ~a | ~a~%"
          (~a b #:width 2 #:align 'right)
          (~a a #:width 6 #:align 'right)
          (~a p)))

;;; display the table
(display-row "#" "Angle" "Point")
(displayln "---+--------+-------------------")
(for ((heading headings-list))
     (let* ((box-number (heading->box heading)))
       ;; by coincidence, default value of the second argument to
       ;; real->decimal-string "decimal-digits" is 2,... just what we want!
       (display-row box-number
                    (real->decimal-string heading)
                    (list-ref box-names (sub1 box-number)))))

(module+ test
 (require rackunit)
 ;;; unit test heading->box (the business end of the implementation)
 (check-= (heading->box 354.38) 1 0)
 (check-= (heading->box 5.62) 1 0)
 (check-= (heading->box 5.63) 2 0)
 (check-= (heading->box 16.87) 2 0))
