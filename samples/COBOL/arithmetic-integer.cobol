       IDENTIFICATION DIVISION.
       PROGRAM-ID. Int-Arithmetic.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 A      PIC S9(10).
       01 B      PIC S9(10).
       01 Result PIC S9(10).

       PROCEDURE DIVISION.
           DISPLAY "First number: " WITH NO ADVANCING
           ACCEPT A
           DISPLAY "Second number: " WITH NO ADVANCING
           ACCEPT B

*          *> Note: The various ADD/SUBTRACT/etc. statements can be
*          *> replaced with COMPUTE statements, which allow those
*          *> operations to be defined similarly to other languages,
*          *> e.g. COMPUTE Result = A + B

           ADD A TO B GIVING Result
           DISPLAY "A + B = " Result

           SUBTRACT B FROM A GIVING Result
           DISPLAY "A - B = " Result

           MULTIPLY A BY B GIVING Result
           DISPLAY "A * B = " Result

*          *> Division here truncates towards zero. DIVIDE can take a
*          *> ROUNDED clause, which will round the result to the nearest
*          *> integer.
           DIVIDE A BY B GIVING Result
           DISPLAY "A / B = " Result

           COMPUTE Result = A ^ B
           DISPLAY "A ^ B = " Result

*          *> Matches sign of first argument.
           DISPLAY "A % B = " FUNCTION REM(A, B)

           GOBACK
           .
