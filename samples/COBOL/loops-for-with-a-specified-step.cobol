       IDENTIFICATION DIVISION.
       PROGRAM-ID. Display-Odd-Nums.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  I PIC 99.

       PROCEDURE DIVISION.
           PERFORM VARYING I FROM 1 BY 2 UNTIL 10 < I
               DISPLAY I
           END-PERFORM

           GOBACK
           .
