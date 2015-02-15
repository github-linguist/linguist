#lang racket

(require plot plot/utils)

(plot (points (for*/lists (result)
                ([_ (in-naturals)]
                 #:break (= 100 (length result))
                 [xy (in-value (v- (vector (random 31) (random 31))
                                   #(15 15)))]
                 #:when (<= 10 (vmag xy) 15))
                xy)))
