       IDENTIFICATION DIVISION.
       PROGRAM-ID. GCD.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A        PIC 9(10)   VALUE ZEROES.
       01 B        PIC 9(10)   VALUE ZEROES.
       01 TEMP     PIC 9(10)   VALUE ZEROES.

       PROCEDURE DIVISION.
       Begin.
           DISPLAY "Enter first number, max 10 digits."
           ACCEPT A
           DISPLAY "Enter second number, max 10 digits."
           ACCEPT B
           IF A < B
             MOVE B TO TEMP
             MOVE A TO B
             MOVE TEMP TO B
           END-IF

           PERFORM UNTIL B = 0
             MOVE A TO TEMP
             MOVE B TO A
             DIVIDE TEMP BY B GIVING TEMP REMAINDER B
           END-PERFORM
           DISPLAY "The gcd is " A
           STOP RUN.
