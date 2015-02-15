       IDENTIFICATION DIVISION.
       PROGRAM-ID. Int-Compare.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  A PIC 9(10).
       01  B PIC 9(10).

       PROCEDURE DIVISION.
           DISPLAY "First number: " WITH NO ADVANCING
           ACCEPT A
           DISPLAY "Second number: " WITH NO ADVANCING
           ACCEPT B

*          *> Note: Longer verbal forms may be used instead of symbols
*          *> e.g. 'IS GREATER THAN' instead '<'
           IF A < B
               DISPLAY A " is less than " B
           ELSE IF A = B
               DISPLAY A " is equal to " B
           ELSE IF A > B
               DISPLAY A " is larger than " B
           END-IF

           GOBACK
           .
