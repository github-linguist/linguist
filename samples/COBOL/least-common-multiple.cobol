       IDENTIFICATION DIVISION.
       PROGRAM-ID. show-lcm.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION lcm
           .
       PROCEDURE DIVISION.
           DISPLAY "lcm(35, 21) = " FUNCTION lcm(35, 21)
           GOBACK
           .
       END PROGRAM show-lcm.

       IDENTIFICATION DIVISION.
       FUNCTION-ID. lcm.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION gcd
           .
       DATA DIVISION.
       LINKAGE SECTION.
       01  m                       PIC S9(8).
       01  n                       PIC S9(8).
       01  ret                     PIC S9(8).

       PROCEDURE DIVISION USING VALUE m, n RETURNING ret.
           COMPUTE ret = FUNCTION ABS(m * n) / FUNCTION gcd(m, n)
           GOBACK
           .
       END FUNCTION lcm.

       IDENTIFICATION DIVISION.
       FUNCTION-ID. gcd.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  temp                    PIC S9(8).

       01  x                       PIC S9(8).
       01  y                       PIC S9(8).

       LINKAGE SECTION.
       01  m                       PIC S9(8).
       01  n                       PIC S9(8).
       01  ret                     PIC S9(8).

       PROCEDURE DIVISION USING VALUE m, n RETURNING ret.
           MOVE m to x
           MOVE n to y

           PERFORM UNTIL y = 0
               MOVE x TO temp
               MOVE y TO x
               MOVE FUNCTION MOD(temp, y) TO Y
           END-PERFORM

           MOVE FUNCTION ABS(x) TO ret
           GOBACK
           .
       END FUNCTION gcd.
