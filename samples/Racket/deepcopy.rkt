#lang racket

(define (deepcopy x)
  ;; make sure that all sharings are shown
  (parameterize ([print-graph #t]) (read (open-input-string (format "~s" x)))))

(define (try x)
  ;; use the same setting to see that it worked
  (parameterize ([print-graph #t])
    (printf "original: ~s\n" x)
    (printf "deepcopy: ~s\n" (deepcopy x))
    ;; print both also, which shows that they are indeed different
    (printf "both: ~s\n" (list x (deepcopy x)))))
(try (shared ([x (cons 1 x)]) (list x x)))
