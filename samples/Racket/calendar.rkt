#lang racket
(require racket/date)
(define (calendar yr)
  (define (nsplit n l) (if (null? l) l (cons (take l n) (nsplit n (drop l n)))))
  (define months
    (for/list ([mn (in-naturals 1)]
               [mname '(January February March April May June July
                        August September October November December)])
      (define s (find-seconds 0 0 12 1 mn yr))
      (define pfx (date-week-day (seconds->date s)))
      (define days
        (let ([? (if (= mn 12) (λ(x y) y) (λ(x y) x))])
          (round (/ (- (find-seconds 0 0 12 1 (? (+ 1 mn) 1) (? yr (+ 1 yr))) s)
                    60 60 24))))
      (list* (~a mname #:width 20 #:align 'center) "Su Mo Tu We Th Fr Sa"
             (map string-join
                  (nsplit 7 `(,@(make-list pfx "  ")
                              ,@(for/list ([d days])
                                  (~a (+ d 1) #:width 2 #:align 'right))
                              ,@(make-list (- 42 pfx days) "  ")))))))
  (let* ([s '(" 11,-~4-._3. 41-4! 10/ ()=(2) 3\\ 40~a! 9( 3( 80 39-4! 10\\._\\"
              ", ,-4'! 5#2X3x7! 12/ 2-3'~2;! 11/ 4/~2|-! 9=( 3~4 2|! 3/~42\\! "
              "2/_23\\! /_25\\!/_27\\! 3|_20|! 3|_20|! 3|_20|! 3| 20|!!")]
         [s (regexp-replace* #rx"!" (string-append* s) "\n")]
         [s (regexp-replace* #rx".(?:[1-7][0-9]*|[1-9])" s
              (λ(m) (make-string (string->number (substring m 1))
                                 (string-ref m 0))))])
    (printf s yr))
  (for-each displayln
    (dropf-right (for*/list ([3ms (nsplit 3 months)] [s (apply map list 3ms)])
                   (regexp-replace #rx" +$" (string-join s "   ") ""))
                 (λ(s) (equal? "" s)))))

(calendar 1969)
