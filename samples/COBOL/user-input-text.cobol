       IDENTIFICATION DIVISION.
       PROGRAM-ID. Get-Input.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Input-String PIC X(30).
       01  Input-Int    PIC 9(5).

       PROCEDURE DIVISION.
       DISPLAY "Enter a string:"
       ACCEPT Input-String

       DISPLAY "Enter a number:"
       ACCEPT Input-Int

       GOBACK
       .
