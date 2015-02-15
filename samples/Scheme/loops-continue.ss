(define (loop i)
  (if (> i 10) 'done
      (begin
       (display i)
       (cond ((zero? (modulo i 5))
              (newline) (loop (+ 1 i)))
             (else (display ", ")
                   (loop (+ 1 i)))))))
