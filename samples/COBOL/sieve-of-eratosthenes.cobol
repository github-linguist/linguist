*> Please ignore the asterisks in the first column of the next comments,
*> which are kludges to get syntax highlighting to work.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Sieve-Of-Eratosthenes.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  Max-Number       USAGE UNSIGNED-INT.
       01  Max-Prime        USAGE UNSIGNED-INT.

       01  Num-Group.
           03  Num-Table PIC X VALUE "P"
                   OCCURS 1 TO 10000000 TIMES DEPENDING ON Max-Number
                   INDEXED BY Num-Index.
               88  Is-Prime VALUE "P" FALSE "N".

       01  Current-Prime    USAGE UNSIGNED-INT.

       01  I                USAGE UNSIGNED-INT.

       PROCEDURE DIVISION.
           DISPLAY "Enter the limit: " WITH NO ADVANCING
           ACCEPT Max-Number
           DIVIDE Max-Number BY 2 GIVING Max-Prime

*          *> Set Is-Prime of all non-prime numbers to false.
           SET Is-Prime (1) TO FALSE
           PERFORM UNTIL Max-Prime < Current-Prime
*              *> Set current-prime to next prime.
               ADD 1 TO Current-Prime
               PERFORM VARYING Num-Index FROM Current-Prime BY 1
                   UNTIL Is-Prime (Num-Index)
               END-PERFORM
               MOVE Num-Index TO Current-Prime

*              *> Set Is-Prime of all multiples of current-prime to
*              *> false, starting from current-prime sqaured.
               COMPUTE Num-Index = Current-Prime ** 2
               PERFORM UNTIL Max-Number < Num-Index
                   SET Is-Prime (Num-Index) TO FALSE
                   SET Num-Index UP BY Current-Prime
               END-PERFORM
           END-PERFORM

*          *> Display the prime numbers.
           PERFORM VARYING Num-Index FROM 1 BY 1
                   UNTIL Max-Number < Num-Index
               IF Is-Prime (Num-Index)
                   DISPLAY Num-Index
               END-IF
           END-PERFORM

           GOBACK
           .
