#lang racket
(require ffi/unsafe ffi/unsafe/define)
(define-ffi-definer defmm (ffi-lib "Winmm"))
(defmm midiOutOpen (_fun [h : (_ptr o _int32)] [_int = -1] [_pointer = #f]
                         [_pointer = #f] [_int32 = 0] -> _void -> h))
(defmm midiOutShortMsg (_fun _int32 _int32 -> _void))
(define M (midiOutOpen))
(define (midi x y z) (midiOutShortMsg M (+ x (* 256 y) (* 65536 z))))

(define raw-codes
  '("a.-|b-...|c-.-.|d-..|e.|f..-.|g--.|h....|i..|j.---|k-.-|l.-..|m--|n-."
    "|o---|p--.-|q--.-|r.-.|s...|t-|u..-|v...-|w.--|x-..-|y-.--|z--..|1.----"
    "|2..---|3...--|4....-|5.....|6-....|7--...|8---..|9----.|0-----"))

(define codes
  (for/list ([x (regexp-split #rx"\\|" (string-append* raw-codes))])
    (cons (string-ref x 0) (substring x 1))))

(define (morse str [unit 0.1])
  (define (sound len)
    (midi #x90 72 127) (sleep (* len unit))
    (midi #x90 72 0)   (sleep unit))
  (define (play str)
    (midi #xC0 #x35 0) ; use a cute voice
    (for ([c str])
      (case c [(#\.) (sound 1)] [(#\-) (sound 3)] [(#\ ) (sleep (* 3 unit))])))
  (let* ([str (string-foldcase str)]
         [str (regexp-replace* #rx"[,:;]+" str " ")]
         [str (regexp-replace* #rx"[.!?]+" str ".")]
         [str (string-normalize-spaces str)])
    (for ([s (string-split str)])
      (define m
        (string-join
         (for/list ([c s])
           (cond [(assq c codes) => cdr]
                 [else (case c [(#\space) " "] [(#\.) "  "] [else ""])]))))
      (printf "~a: ~a\n" s m)
      (play (string-append m "  ")))))

(morse "Say something here")
