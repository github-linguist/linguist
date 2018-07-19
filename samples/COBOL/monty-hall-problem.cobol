       IDENTIFICATION DIVISION.
       PROGRAM-ID. monty-hall.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  Num-Games               VALUE 1000000.

       *> These are needed so the values are passed to
       *> get-rand-int correctly.
       01  One                     PIC 9 VALUE 1.
       01  Three                   PIC 9 VALUE 3.

       01  doors-area.
           03  doors               PIC 9 OCCURS 3 TIMES.

       01  choice                  PIC 9.
       01  shown                   PIC 9.
       01  winner                  PIC 9.

       01  switch-wins             PIC 9(7).
       01  stay-wins               PIC 9(7).

       01  stay-wins-percent       PIC Z9.99.
       01  switch-wins-percent     PIC Z9.99.

       PROCEDURE DIVISION.
           PERFORM Num-Games TIMES
               MOVE 0 TO doors (winner)

               CALL "get-rand-int" USING CONTENT One, Three,
                   REFERENCE winner
               MOVE 1 TO doors (winner)

               CALL "get-rand-int" USING CONTENT One, Three,
                   REFERENCE choice

               PERFORM WITH TEST AFTER
                       UNTIL NOT(shown = winner OR choice)
                   CALL "get-rand-int" USING CONTENT One, Three,
                       REFERENCE shown
               END-PERFORM

               ADD doors (choice) TO stay-wins
               ADD doors (6 - choice - shown) TO switch-wins
           END-PERFORM

           COMPUTE stay-wins-percent ROUNDED =
               stay-wins / Num-Games * 100
           COMPUTE switch-wins-percent ROUNDED =
               switch-wins / Num-Games * 100

           DISPLAY "Staying wins   " stay-wins " times ("
               stay-wins-percent "%)."
           DISPLAY "Switching wins " switch-wins " times ("
               switch-wins-percent "%)."
           .

       IDENTIFICATION DIVISION.
       PROGRAM-ID. get-rand-int.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  call-flag               PIC X VALUE "Y".
           88  first-call          VALUE "Y", FALSE "N".

       01  num-range               PIC 9.

       LINKAGE SECTION.
       01  min-num                 PIC 9.
       01  max-num                 PIC 9.

       01  ret                     PIC 9.

       PROCEDURE DIVISION USING min-num, max-num, ret.
           *> Seed RANDOM once.
           IF first-call
               MOVE FUNCTION RANDOM(FUNCTION CURRENT-DATE (9:8))
                   TO num-range
               SET first-call TO FALSE
           END-IF

           COMPUTE num-range = max-num - min-num + 1
           COMPUTE ret =
              FUNCTION MOD(FUNCTION RANDOM * 100000, num-range)
              + min-num
           .
       END PROGRAM get-rand-int.

       END PROGRAM monty-hall.
