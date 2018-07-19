#CI(MODULE NAME-OF-THIS-FILE RACKET
(REQUIRE RACKET/DATE)
(DEFINE (CALENDAR YR)
  (DEFINE (NSPLIT N L) (IF (NULL? L) L (CONS (TAKE L N) (NSPLIT N (DROP L N)))))
  (DEFINE MONTHS
    (FOR/LIST ([MN (IN-NATURALS 1)]
               [MNAME '(JANUARY FEBRUARY MARCH APRIL MAY JUNE JULY
                        AUGUST SEPTEMBER OCTOBER NOVEMBER DECEMBER)])
      (DEFINE S (FIND-SECONDS 0 0 12 1 MN YR))
      (DEFINE PFX (DATE-WEEK-DAY (SECONDS->DATE S)))
      (DEFINE DAYS
        (LET ([? (IF (= MN 12) (Λ(X Y) Y) (Λ(X Y) X))])
          (ROUND (/ (- (FIND-SECONDS 0 0 12 1 (? (+ 1 MN) 1) (? YR (+ 1 YR))) S)
                    60 60 24))))
      (LIST* (~A MNAME #:WIDTH 20 #:ALIGN 'CENTER) "SU MO TU WE TH FR SA"
             (MAP STRING-JOIN
                  (NSPLIT 7 `(,@(MAKE-LIST PFX "  ")
                              ,@(FOR/LIST ([D DAYS])
                                  (~A (+ D 1) #:WIDTH 2 #:ALIGN 'RIGHT))
                              ,@(MAKE-LIST (- 42 PFX DAYS) "  ")))))))
  (LET* ([S '(" 11,-~4-._3. 41-4! 10/ ()=(2) 3\\ 40~A! 9( 3( 80 39-4! 10\\._\\"
              ", ,-4'! 5#2X3@7! 12/ 2-3'~2;! 11/ 4/~2|-! 9=( 3~4 2|! 3/~42\\! "
              "2/_23\\! /_25\\!/_27\\! 3|_20|! 3|_20|! 3|_20|! 3| 20|!!")]
         [S (REGEXP-REPLACE* "!" (STRING-APPEND* S) "~%")]
         [S (REGEXP-REPLACE* "@" S (STRING-FOLDCASE "X"))]
         [S (REGEXP-REPLACE* ".(?:[1-7][0-9]*|[1-9])" S
              (Λ(M) (MAKE-STRING (STRING->NUMBER (SUBSTRING M 1))
                                 (STRING-REF M 0))))])
    (PRINTF S YR))
  (FOR-EACH (COMPOSE1 DISPLAYLN STRING-TITLECASE)
    (DROPF-RIGHT (FOR*/LIST ([3MS (NSPLIT 3 MONTHS)] [S (APPLY MAP LIST 3MS)])
                   (REGEXP-REPLACE " +$" (STRING-JOIN S "   ") ""))
                 (Λ(S) (EQUAL? "" S)))))

(CALENDAR 1969))
