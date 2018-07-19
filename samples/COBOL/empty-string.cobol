       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EMPTYSTR.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  str                     PIC X(10).

       PROCEDURE DIVISION.
       Begin.

*     *    Assign an empty string.
           INITIALIZE str.

*     *    Or
           MOVE " " TO str.

           IF (str = " ")
              DISPLAY "String is empty"
           ELSE
              DISPLAY "String is not empty"
           END-IF.

           STOP RUN.
