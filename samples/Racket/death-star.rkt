#lang racket
(require plot)
(plot3d (polar3d (λ (φ θ) (real-part (- (sin θ) (sqrt (- (sqr 1/3) (sqr (cos θ)))))))
                 #:samples 100 #:line-style 'transparent #:color 9)
        #:altitude 60 #:angle 80
        #:height  500 #:width 400
        #:x-min  -1/2 #:x-max 1/2
        #:y-min  -1/2 #:y-max 1/2
        #:z-min     0 #:z-max 1)
