       IDENTIFICATION DIVISION.
       PROGRAM-ID. Loop-While.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  I PIC 9999 VALUE 1024.

       PROCEDURE DIVISION.
           PERFORM UNTIL NOT 0 < I
               DISPLAY I
               DIVIDE 2 INTO I
           END-PERFORM

           GOBACK
           .
