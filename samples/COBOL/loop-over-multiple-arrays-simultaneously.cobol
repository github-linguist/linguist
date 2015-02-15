       IDENTIFICATION DIVISION.
       PROGRAM-ID. Loop-Over-Multiple-Tables.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  A VALUE "abc".
           03  A-Vals PIC X OCCURS 3 TIMES.

       01  B VALUE "ABC".
           03  B-Vals PIC X OCCURS 3 TIMES.

       01  C VALUE "123".
           03  C-Vals PIC 9 OCCURS 3 TIMES.

       01  I PIC 9.

       PROCEDURE DIVISION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL 3 < I
               DISPLAY A-Vals (I) B-Vals (I) C-Vals (I)
           END-PERFORM

           GOBACK
           .
