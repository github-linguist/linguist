(define (file-size filename)
  (call-with-input-file filename (lambda (port)
    (let loop ((c (read-char port))
               (count 0))
      (if (eof-object? c)
          count
          (loop (read-char port) (+ 1 count)))))))

(file-size "input.txt")
(file-size "/input.txt")
