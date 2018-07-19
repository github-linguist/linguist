; Get the list reference number for a member or #f if not found
(define (member-string-ref m l)
  (define r #f)
  (let loop ((i 0))
    (if (< i (length l))
        (if (not (string=? (list-ref l i) m))
            (loop (+ i 1))
            (set! r i))))
  r)

;; Compress a string with LZW
(define (lzw-compress uncompressed)
  (define dictionary '())
  (define n 0)
  (define result '())
  (set! uncompressed (string->list uncompressed))

  ;; Setup Dictionary
  (let dict-setup ((c 0))
    (if (> 256 c)
        (begin
          (set! dictionary (append dictionary
                                   (list (string (integer->char c)))))
          (set! n (+ n 1))
          (dict-setup (+ c 1)))))

  ;; Compress the string
  (let compress ((w "") (ci 0))
    (define c (string (list-ref uncompressed ci)))
    (define wc "")
    (set! wc (string-append w c))
    (if (member-string-ref wc dictionary)
        (set! w wc)
        (begin
          (set! result (append result
                               (list (member-string-ref w dictionary))))
          (set! dictionary (append dictionary (list wc)))
          (set! n (+ n 1))
          (set! w c)))
    (if (eqv? ci (- (length uncompressed) 1))
        (set! result (append result
                             (list (member-string-ref w dictionary))))
        (compress w (+ ci 1))))
  result)

;; Decompress a LZW compressed string (input should be a list of integers)
(define (lzw-decompress compressed)
  (define dictionary '())
  (define n 0)
  (define result "")

  ;; Setup Dictionary
  (let dict-setup ((c 0))
    (if (> 256 c)
        (begin
          (set! dictionary (append dictionary
                                   (list (string (integer->char c)))))
          (set! n (+ n 1))
          (dict-setup (+ c 1)))))

  ;; Decompress the list
  (let decompress ((k (list-ref compressed 0)) (ci 0))
    (define kn #f)
    ;; Add to dictionary
    (if (> (length compressed) (+ ci 1))
        (begin
          (set! kn (list-ref compressed (+ ci 1)))
          (if (< kn (length dictionary))
              (set! dictionary
                    (append dictionary
                            (list (string-append
				   (list-ref dictionary k)
				   (string (string-ref (list-ref dictionary kn) 0)))))))))

    ;; Build the resulting string
    (set! result (string-append result (list-ref dictionary k)))

    (if (not (eqv? ci (- (length compressed) 1)))
        (decompress kn (+ ci 1))))
  result)

(define compressed (lzw-compress "TOBEORNOTTOBEORTOBEORNOT"))
(display compressed) (newline)
(define decompressed (lzw-decompress compressed))
(display decompressed) (newline)
