(define earth-radius 6371)
(define pi (acos -1))

(define (distance lat1 long1 lat2 long2)
(define (h a b) (expt (sin (/ (- b a) 2)) 2))
(* 2 earth-radius (asin (sqrt (+ (h lat1 lat2) (* (cos lat1) (cos lat2) (h long1 long2)))))))

(define (deg-to-rad d m s) (* (/ pi 180) (+ d (/ m 60) (/ s 3600))))

(distance (deg-to-rad 36  7.2 0) (deg-to-rad  86 40.2 0)
          (deg-to-rad 33 56.4 0) (deg-to-rad 118 24.0 0))
; 2886.444442837984
