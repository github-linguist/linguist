#lang racket

(define outs (list (current-output-port)))
(define ((tell-all who o) line)
  (for ([c outs] #:unless (eq? o c)) (displayln (~a who ": " line) c)))

(define ((client i o))
  (define nick (begin (display "Nick: " o) (read-line i)))
  (define tell (tell-all nick o))
  (let loop ([line "(joined)"])
    (if (eof-object? line)
      (begin (tell "(left)") (set! outs (remq o outs)) (close-output-port o))
      (begin (tell line) (loop (read-line i))))))

(define (chat-server listener)
  (define-values [i o] (tcp-accept listener))
  (for ([p (list i o)]) (file-stream-buffer-mode p 'none))
  (thread (client i o)) (set! outs (cons o outs)) (chat-server listener))

(void (thread (λ() (chat-server (tcp-listen 12321)))))
((client (current-input-port) (current-output-port)))
