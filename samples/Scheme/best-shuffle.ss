(define count
  (lambda (str1 str2)
    (let ((len (string-length str1)))
      (let loop ((index 0)
                 (result 0))
        (if (= index len)
            result
            (loop (+ index 1)
                  (if (eq? (string-ref str1 index)
                           (string-ref str2 index))
                      (+ result 1)
                      result)))))))

(define swap
  (lambda (str index1 index2)
    (let ((mutable (string-copy str))
          (char1 (string-ref str index1))
          (char2 (string-ref str index2)))
      (string-set! mutable index1 char2)
      (string-set! mutable index2 char1)
      mutable)))

(define shift
  (lambda (str)
    (string-append (substring str 1 (string-length str))
                   (substring str 0 1))))

(define shuffle
  (lambda (str)
    (let* ((mutable (shift str))
           (len (string-length mutable))
           (max-index (- len 1)))
      (let outer ((index1 0)
                  (best mutable)
                  (best-count (count str mutable)))
        (if (or (< max-index index1)
                (= best-count 0))
            best
            (let inner ((index2 (+ index1 1))
                        (best best)
                        (best-count best-count))
              (if (= len index2)
                  (outer (+ index1 1)
                         best
                         best-count)
                  (let* ((next-mutable (swap best index1 index2))
                         (next-count (count str next-mutable)))
                    (if (= 0 next-count)
                        next-mutable
                        (if (< next-count best-count)
                            (inner (+ index2 1)
                                   next-mutable
                                   next-count)
                            (inner (+ index2 1)
                                   best
                                   best-count)))))))))))


(for-each
 (lambda (str)
   (let ((shuffled (shuffle str)))
     (display
      (string-append str " " shuffled " ("
                     (number->string (count str shuffled)) ")\n"))))
 '("abracadabra" "seesaw" "elk" "grrrrrr" "up" "a"))
