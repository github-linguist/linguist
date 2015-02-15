(define (for-loop start end step func)
  (let loop ((i start))
    (cond ((< i end)
	   (func i)
	   (loop (+ i step))))))

(for-loop 2 9 2
  (lambda (i)
    (display i)
    (newline)))
