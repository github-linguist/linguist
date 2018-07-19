;P3
(define (bitmap->ppm bitmap output-port)
  (define height (send bitmap get-height))
  (define width (send bitmap get-width))
  (define buffer (make-bytes (* width height 4))) ;buffer for storing argb data
  (send bitmap get-argb-pixels 0 0 width height buffer) ;copy pixels
  (parameterize ([current-output-port output-port])
    (printf "P3\n~a ~a\n255" width height) ;header
    (for ([i (* width height)])
      (define pixel-position (* 4 i))
      (when (= (modulo i width) 0) (printf "\n")) ;end of row
      (printf "~s ~s ~s "
              (bytes-ref buffer (+ pixel-position 1)) ;r
              (bytes-ref buffer (+ pixel-position 2)) ;g
              (bytes-ref buffer (+ pixel-position 3)))))) ;b


(call-with-output-file "image.ppm" #:exists 'replace #:mode 'text
  (lambda (out)
    (bitmap->ppm bm out)))

; P6
(define (bitmap->ppm bitmap output-port)
  (define height (send bitmap get-height))
  (define width (send bitmap get-width))
  (define buffer (make-bytes (* width height 4))) ;buffer for storing argb data
  (send bitmap get-argb-pixels 0 0 width height buffer) ;copy pixels
  (parameterize ([current-output-port output-port])
    (printf "P6\n~a ~a\n255\n" width height) ;header
    (for ([i (* width height)])
      (define pixel-position (* 4 i))
      (write-byte (bytes-ref buffer (+ pixel-position 1))) ; r
      (write-byte (bytes-ref buffer (+ pixel-position 2))) ; g
      (write-byte (bytes-ref buffer (+ pixel-position 3)))))) ;b

(call-with-output-file "image.ppm" #:exists 'replace #:mode 'binary
  (lambda (out)
    (bitmap->ppm bm out)))

;or any other output port
