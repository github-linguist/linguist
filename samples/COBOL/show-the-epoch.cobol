       IDENTIFICATION DIVISION.
       PROGRAM-ID. epoch.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  epoch-date.
           03  year                PIC 9(4).
           03  month               PIC 99.
           03  dday                PIC 99.

       PROCEDURE DIVISION.
           MOVE FUNCTION DATE-OF-INTEGER(1) TO epoch-date

           DISPLAY year "-" month "-" dday

           GOBACK
           .
