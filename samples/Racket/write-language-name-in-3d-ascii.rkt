#lang racket/gui

;; Get the language name
(define str (cadr (regexp-match #rx"Welcome to (.*?) *v[0-9.]+\n*$" (banner))))

;; Font to use
(define font (make-object font% 12 "MiscFixed" 'decorative 'normal 'bold))
;; (get-face-list) -> get a list of faces to try in the above

;; Calculate the needed size (leave space for a drop-down shadow)
(define-values [W H]
  (let ([bdc (make-object bitmap-dc% (make-object bitmap% 1 1 #t))])
    (call-with-values
        (λ() (send* bdc (set-font font) (get-text-extent str font)))
        (λ(w h _1 _2) (values (+ 2 (inexact->exact (round w)))
                              (+ 2 (inexact->exact (round h))))))))

;; Draw the text
(define bmp (make-bitmap W H #t))
(define dc (send bmp make-dc))
(send* dc (set-font font) (draw-text str 2 0))

;; Grab the pixels as a string, 3d-ed with "/"s
(define scr
  (let* ([size (* W H 4)] [buf (make-bytes size)])
    (send bmp get-argb-pixels 0 0 W H buf)
    (define scr (make-string (* (add1 W) (add1 H)) #\space))
    (for ([i (in-range 0 size 4)] [j (* W H)]
          #:unless (zero? (bytes-ref buf i)))
      (string-set! scr j #\@)
      (for ([k (list j (+ j W -1))] [c "/."] #:when #t
            [k (list (- k 1) (+ k W) (+ k W -1))]
            #:when (and (< -1 k (string-length scr))
                        (member (string-ref scr k) '(#\space #\.))))
        (string-set! scr k c)))
    scr))

;; Show it, dropping empty lines
(let ([lines (for/list ([y H]) (substring scr (* y W) (* (add1 y) W)))])
  (define (empty? l) (not (regexp-match #rx"[^ ]" l)))
  (for ([line (dropf-right (dropf lines empty?) empty?)])
    (displayln (string-trim line #:left? #f))))
