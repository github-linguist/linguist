       IDENTIFICATION DIVISION.
       PROGRAM-ID. A-Plus-B.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  A       PIC S9(5).
       01  B       PIC S9(5).

       01  A-B-Sum PIC S9(5).

       PROCEDURE DIVISION.
           ACCEPT A
           ACCEPT B

           ADD A TO B GIVING A-B-Sum

           DISPLAY A-B-Sum

           GOBACK
           .
