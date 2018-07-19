       IDENTIFICATION DIVISION.
       PROGRAM-ID. Guess-With-Feedback.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  Seed       PIC 9(8).
       01  Random-Num PIC 99.
       01  Guess      PIC 99.

       PROCEDURE DIVISION.
           ACCEPT Seed FROM TIME
           COMPUTE Random-Num =
               FUNCTION REM(FUNCTION RANDOM(Seed) * 1000, 10) + 1

           DISPLAY "Guess a number between 1 and 10:"

           PERFORM FOREVER
               ACCEPT Guess

               IF Guess > Random-Num
                   DISPLAY "Your guess was too high."
               ELSE IF Guess < Random-Num
                   DISPLAY "Your guess was too low."
               ELSE
                   DISPLAY "Well guessed!"
                   EXIT PERFORM
           END-PERFORM

           GOBACK
           .
