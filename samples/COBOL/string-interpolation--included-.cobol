       IDENTIFICATION DIVISION.
       PROGRAM-ID. interpolation-included.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  extra PIC X(6) VALUE "little".

       PROCEDURE DIVISION.
           DISPLAY FUNCTION SUBSTITUTE("Mary had a X lamb.", "X", extra)

           GOBACK
           .
