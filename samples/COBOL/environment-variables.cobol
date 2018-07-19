       IDENTIFICATION DIVISION.
       PROGRAM-ID. Environment-Vars.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  home PIC X(75).

       PROCEDURE DIVISION.
*          *> Method 1.
           ACCEPT home FROM ENVIRONMENT "HOME"
           DISPLAY home

*          *> Method 2.
           DISPLAY "HOME" UPON ENVIRONMENT-NAME
           ACCEPT home FROM ENVIRONMENT-VALUE

           GOBACK
           .
