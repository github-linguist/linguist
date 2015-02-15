       IDENTIFICATION DIVISION.
       PROGRAM-ID. loop-continue.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  i PIC 99.

       PROCEDURE DIVISION.
           PERFORM VARYING i FROM 1 BY 1 UNTIL 10 < i
               DISPLAY i WITH NO ADVANCING

               IF FUNCTION MOD(i, 5) = 0
                   DISPLAY SPACE
                   EXIT PERFORM CYCLE
               END-IF

               DISPLAY ", " WITH NO ADVANCING
           END-PERFORM

           GOBACK
           .
