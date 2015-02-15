       IDENTIFICATION DIVISION.
       PROGRAM-ID. leap-year.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  examples VALUE "19001994199619972000".
           03  year PIC 9(4) OCCURS 5 TIMES
               INDEXED BY year-index.

       01  remainders.
           03 400-rem   PIC 9(4).
           03 100-rem   PIC 9(4).
           03 4-rem     PIC 9(4).

       PROCEDURE DIVISION.
           PERFORM VARYING year-index FROM 1 BY 1 UNTIL 5 < year-index
               MOVE FUNCTION MOD(year (year-index), 400) TO 400-rem
               MOVE FUNCTION MOD(year (year-index), 100) TO 100-rem
               MOVE FUNCTION MOD(year (year-index), 4) TO 4-rem

               IF 400-rem = 0 OR ((100-rem NOT = 0) AND 4-rem = 0)
                   DISPLAY year (year-index) " is a leap year."
               ELSE
                   DISPLAY year (year-index) " is not a leap year."
               END-IF
           END-PERFORM

           GOBACK
           .
