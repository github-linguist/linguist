       IDENTIFICATION DIVISION.
       PROGRAM-ID. Random-Nums.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Num  PIC Z9.

       PROCEDURE DIVISION.
       Main.
           PERFORM FOREVER
               PERFORM Generate-And-Display-Num

               IF Num = 10
                   EXIT PERFORM
               ELSE
                   PERFORM Generate-And-Display-Num
               END-IF
           END-PERFORM

           GOBACK
           .

       Generate-And-Display-Num.
           COMPUTE Num =  FUNCTION REM(FUNCTION RANDOM * 100, 20)
           DISPLAY Num
           .
