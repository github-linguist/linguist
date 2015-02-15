(define ((simple-moving-averager size . nums) num)
  (set! nums (cons num (if (= (length nums) size) (reverse (cdr (reverse nums))) nums)))
  (/ (apply + nums) (length nums)))

(define av (simple-moving-averager 3))
(map av '(1 2 3 4 5 5 4 3 2 1))
