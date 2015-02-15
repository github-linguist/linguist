(define reader->printer-channel (make-channel))
(define printer->reader-channel (make-channel))

(define (sync-line-counter filename)
  (define (reader)
    (define file-port (open-input-file filename))
    (let loop ([line (read-line file-port)])
      (when (not (eof-object? line))
        (begin
          (channel-put reader->printer-channel line)
          (loop (read-line file-port)))))
    (channel-put reader->printer-channel eof)
    (let ([num-lines (channel-get printer->reader-channel)])
      (printf "Number of lines printed = ~a~%" num-lines)))

  (define (printer)
    (define count 0)
    (let loop ([line (channel-get reader->printer-channel)])
      (when (not (eof-object? line))
        (begin
          (printf "~a~%" line)
          (set! count (add1 count))
          (loop (channel-get reader->printer-channel)))))
    (channel-put printer->reader-channel count))

  (thread reader)
  (thread printer))

(sync-line-counter "input.txt")
