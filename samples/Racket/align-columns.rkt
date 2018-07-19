#lang racket

(define (display-aligned text #:justify [justify 'left])
  (define lines
    (for/list ([line (regexp-split #rx"\n" text)])
      (regexp-split #rx"\\$" line)))
  (define width
    (add1 (for*/fold ([m 0]) ([line lines] [word line])
            (max m (string-length word)))))
  (define spaces (make-string width #\space))
  (for ([line lines])
    (for* ([word line]
           [strs (let ([spc (substring spaces (string-length word))])
                   (case justify
                     [(left)  (list word spc)]
                     [(right) (list spc word)]
                     [(center) (let ([i (quotient (string-length spc) 2)])
                                 (list (substring spc i)
                                       word
                                       (substring spc 0 i)))]))])
      (display strs))
    (newline)))

(define text
  "Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.")

(display-aligned text)
(display-aligned #:justify 'right text)
(display-aligned #:justify 'center text)
