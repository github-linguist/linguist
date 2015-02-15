#lang racket

(define-syntax-rule (with-raw body ...)
  (let ([saved #f])
    (define (stty x) (system (~a "stty " x)) (void))
    (dynamic-wind (λ() (set! saved (with-output-to-string (λ() (stty "-g"))))
                       (stty "raw -echo opost"))
                  (λ() body ...)
                  (λ() (stty saved)))))

(define (->bytes x)
  (cond [(bytes? x)  x]
        [(string? x) (string->bytes/utf-8 x)]
        [(not (list? x)) (error '->bytes "don't know how to convert: ~e" x)]
        [(andmap byte? x) (list->bytes x)]
        [(andmap char? x) (->bytes (list->string x))]))
(define (open x)
  (open-input-bytes (->bytes x)))

(define macros (make-vector 256 #f))
(define (macro-set! seq expansion)
  (let loop ([bs (bytes->list (->bytes seq))] [v (vector macros)] [i 0])
    (if (null? bs)
      (vector-set! v i expansion)
      (begin (unless (vector-ref v i) (vector-set! v i (make-vector 256 #f)))
             (loop (cdr bs) (vector-ref v i) (car bs))))))

;; Some examples
(macro-set! "\3" exit)
(macro-set! "ME" "Random J. Hacker")
(macro-set! "EMAIL" (λ() (open "ME <me@example.com>")))
(macro-set! "\r" "\r\n")
(macro-set! "\n" "\r\n")
(for ([c "ABCD"]) (macro-set! (~a "\eO" c) (~a "\e[" c)))

(with-raw
  (printf "Type away, `C-c' to exit...\n")
  (let loop ([inps (list (current-input-port))] [v macros] [pending '()])
    (define b (read-byte (car inps)))
    (if (eq? b eof) (loop (cdr inps) v pending)
        (let mloop ([m (vector-ref v b)])
          (cond [(vector? m) (loop inps m (cons b pending))]
                [(input-port? m) (loop (cons m inps) macros '())]
                [(or (bytes? m) (string? m))
                 (display m) (flush-output) (loop inps macros '())]
                [(procedure? m) (mloop (m))]
                [(and m (not (void? m))) (error "bad macro mapping!")]
                [(pair? pending)
                 (define rp (reverse (cons b pending)))
                 (write-byte (car rp)) (flush-output)
                 (loop (if (null? (cdr rp)) inps
                           (cons (open (list->bytes (cdr rp))) inps))
                       macros '())]
                [else (write-byte b) (flush-output) (loop inps v '())])))))
