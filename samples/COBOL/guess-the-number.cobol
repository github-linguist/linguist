       IDENTIFICATION DIVISION.
       PROGRAM-ID. Guess-The-Number.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Random-Num PIC 99.
       01  Guess      PIC 99.

       PROCEDURE DIVISION.
           COMPUTE Random-Num = 1 + (FUNCTION RANDOM * 10)
           DISPLAY "Guess a number between 1 and 10:"

           PERFORM FOREVER
               ACCEPT Guess

               IF Guess = Random-Num
                   DISPLAY "Well guessed!"
                   EXIT PERFORM
               ELSE
                   DISPLAY "That isn't it. Try again."
               END-IF
           END-PERFORM

           GOBACK
           .
