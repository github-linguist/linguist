#!/usr/local/bin/gosh

(use srfi-1)  ;; iota

(define *freqs* (make-vector 256 0))

(define (main args)
    (with-input-from-file "../word-list.txt" count-char-freqs)
    (show-char-freqs #\a #\z))

(define (count-char-freqs)
    (let* ((ln (read-line))
           (at-eof (eof-object? ln)))
        (if (not at-eof)
            (let ((string-chars (string->list ln)))
                (for-each count-char-freq string-chars)
                (count-char-freqs)))))

(define (count-char-freq ch)
    (if (char-alphabetic? ch)
        (let* ((char-num (char->integer (char-downcase ch)))
               (frq (vector-ref *freqs* char-num)))
            (vector-set! *freqs* char-num (+ 1 frq)))))

(define (show-char-freqs first-letter last-letter)
    (format #t "Letter Frequency~%")
    (let* ((first-num (char->integer first-letter))
           (last-num (char->integer last-letter))
           (num-count (+ 1 (- last-num first-num)))
           (nums-list (iota num-count first-num)))
        (for-each show-char-freq nums-list)))

(define (show-char-freq let-num)
    (let ((ch (integer->char let-num))
          (frq (vector-ref *freqs* let-num)))
        (format #t "~6a ~8a~%" ch frq)))
