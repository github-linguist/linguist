#lang typed/racket

(require plot/typed)
(plot3d (polar3d (λ (θ ρ) 1)) #:altitude 25)
