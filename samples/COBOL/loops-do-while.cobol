       IDENTIFICATION DIVISION.
       PROGRAM-ID. loop-do-while.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  i PIC 99 VALUE 0.

       PROCEDURE DIVISION.
           PERFORM WITH TEST AFTER UNTIL FUNCTION MOD(i, 6) = 0
               ADD 1 TO i
               DISPLAY i
           END-PERFORM

           GOBACK
           .
