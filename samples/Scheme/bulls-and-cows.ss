;generate a random non-repeating list of 4 digits, 1-9 inclusive
(define (get-num)
  (define (gen lst)
    (if (= (length lst) 4) lst
        (let ((digit (+ (random 9) 1)))
          (if (member digit lst) ;make sure the new digit isn't in the
                                 ;list
              (gen lst)
              (gen (cons digit lst))))))
  (string->list (apply string-append (map number->string (gen '())))))

;is g a valid guess (that is, non-repeating, four digits 1-9
;inclusive?)
(define (valid-guess? g)
  (let ((g-num (string->number (apply string g))))
    ;does the same digit appear twice in lst?
    (define (repeats? lst)
      (cond ((null? lst) #f)
            ((member (car lst) (cdr lst)) #t)
            (else (repeats? (cdr lst)))))
    (and g-num
         (> g-num 1233)
         (< g-num 9877)
         (not (repeats? g)))))

;return '(cows bulls) for the given guess
(define (score answer guess)
  ;total cows + bulls
  (define (cows&bulls a g)
    (cond ((null? a) 0)
          ((member (car a) g) (+ 1 (cows&bulls (cdr a) g)))
          (else (cows&bulls (cdr a) g))))
  ;bulls only
  (define (bulls a g)
    (cond ((null? a) 0)
          ((equal? (car a) (car g)) (+ 1 (bulls (cdr a) (cdr g))))
          (else (bulls (cdr a) (cdr g)))))
  (list (- (cows&bulls answer guess) (bulls answer guess)) (bulls answer guess)))

;play the game
(define (bull-cow answer)
  ;get the user's guess as a list
  (define (get-guess)
    (let ((e (read)))
      (if (number? e)
          (string->list (number->string e))
          (string->list (symbol->string e)))))
  (display "Enter a guess: ")
  (let ((guess (get-guess)))
    (if (valid-guess? guess)
        (let ((bulls (cadr (score answer guess)))
              (cows (car (score answer guess))))
          (if (= bulls 4)
              (display "You win!\n")
              (begin
                (display bulls)
                (display " bulls, ")
                (display cows)
                (display " cows.\n")
                (bull-cow answer))))
        (begin
          (display "Invalid guess.\n")
          (bull-cow answer)))))

(bull-cow (get-num))
