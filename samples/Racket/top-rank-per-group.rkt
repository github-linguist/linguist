#lang racket

(struct employee (name id salary dept))
(define employees
  (list (employee "Tyler Bennett"   "E10297" 32000 "D101")
        (employee "John Rappl"      "E21437" 47000 "D050")
        (employee "George Woltman"  "E00127" 53500 "D101")
        (employee "Adam Smith"      "E63535" 18000 "D202")
        (employee "Claire Buckman"  "E39876" 27800 "D202")
        (employee "David McClellan" "E04242" 41500 "D101")
        (employee "Rich Holcomb"    "E01234" 49500 "D202")
        (employee "Nathan Adams"    "E41298" 21900 "D050")
        (employee "Richard Potter"  "E43128" 15900 "D101")
        (employee "David Motsinger" "E27002" 19250 "D202")
        (employee "Tim Sampair"     "E03033" 27000 "D101")
        (employee "Kim Arlich"      "E10001" 57000 "D190")
        (employee "Timothy Grove"   "E16398" 29900 "D190")))

(define (top/dept N)
  (for/list ([dept (remove-duplicates (map employee-dept employees))])
    (define people
      (filter (λ(e) (equal? dept (employee-dept e))) employees))
    (cons dept (take (sort people > #:key employee-salary) N))))

(for ([dept (top/dept 2)])
  (printf "Department ~a:\n" (car dept))
  (for ([e (cdr dept)])
    (printf "  $~a: ~a (~a)\n"
            (employee-salary e)
            (employee-name e)
            (employee-id e))))
