(define ((running-stddev . nums) num)
  (set! nums (cons num nums))
  (sqrt (- (/ (apply + (map (lambda (i) (* i i)) nums)) (length nums)) (expt (/ (apply + nums) (length nums)) 2))))
