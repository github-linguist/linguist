       IDENTIFICATION DIVISION.
       PROGRAM-ID. Int-Sequence.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
*      *> 36 digits is the largest size a numeric field can have.
       01  I PIC 9(36).

       PROCEDURE DIVISION.
*          *> Display numbers until I overflows.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I = 0
               DISPLAY I
           END-PERFORM

           GOBACK
           .
