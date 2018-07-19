#lang racket/base
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;; Derived from 'D' Implementation
;;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(require racket/date racket/match)

(define seasons '(Chaos     Discord   Confusion Bureaucracy     |The Aftermath|))
(define weekday '(Sweetmorn Boomtime  Pungenday Prickle-Prickle |Setting Orange|))
(define apostle '(Mungday   Mojoday   Syaday    Zaraday         Maladay))
(define holiday '(Chaoflux  Discoflux Confuflux Bureflux        Afflux))

(define (ymd->date y m d) (seconds->date (find-seconds 0 0 0 d m y)))
(define (leap-year? y) (with-handlers ((exn? (λ (x) #f))) (= 29 (date-day (ymd->date y 2 29)))))

(define (discordian-date d)
  (define leap? (leap-year? (date-year d)))
  (define year-day (match* (leap? (date-year-day d))
                     [(#t (? (λ (D) (>= D 59)) d0)) d0]
                     [(_ d0) (add1 d0)]))

  (define season-day (modulo year-day 73)) ; season day
  (define (list-ref-season l)
    (define season-index (quotient year-day 73))
    (symbol->string (list-ref l season-index)))

  (string-append
   (match* (season-day leap? (date-month d) (date-day d))
     [( _ #t 2 29) "St. Tib's Day,"]
     [((app (match-lambda
              (5 apostle) (50 holiday) (_ #f))
            (and (not #f) special)) _ _  _)
      (string-append (list-ref-season special) ",")]
     [( _  _ _  _)
      (define week-day-name (list-ref weekday (modulo (sub1 year-day) 5)))
      (format "~a, day ~a of ~a" week-day-name season-day (list-ref-season seasons))])
   " in the YOLD " (number->string (+ (date-year d) 1166))))

(displayln (discordian-date (current-date)))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; passing these tests makes me consistent with D implementation
(module+ test
  (require rackunit)
  (define discordian/ymd (compose discordian-date ymd->date))
  (check-equal? (discordian/ymd 2010 7 22) "Pungenday, day 57 of Confusion in the YOLD 3176")
  (check-equal? (discordian/ymd 2012 2 28) "Prickle-Prickle, day 59 of Chaos in the YOLD 3178")
  (check-equal? (discordian/ymd 2012 2 29) "St. Tib's Day, in the YOLD 3178");
  (check-equal? (discordian/ymd 2012 3  1) "Setting Orange, day 60 of Chaos in the YOLD 3178")
  (check-equal? (discordian/ymd 2010 1  5) "Mungday, in the YOLD 3176")
  (check-equal? (discordian/ymd 2011 5  3) "Discoflux, in the YOLD 3177"))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~FIN
