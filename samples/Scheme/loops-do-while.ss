(let loop ((i 1))
  (display i)
  (if (positive? (modulo i 6))
      (loop (+ i 1))))
