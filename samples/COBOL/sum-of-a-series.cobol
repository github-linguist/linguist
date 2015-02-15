       IDENTIFICATION DIVISION.
       PROGRAM-ID. sum-of-series.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  N                       VALUE 1000.

       01  series-term             USAGE FLOAT-LONG.
       01  i                       PIC 9(4).

       PROCEDURE DIVISION.
           PERFORM VARYING i FROM 1 BY 1 UNTIL N < i
               COMPUTE series-term = series-term + (1 / i ** 2)
           END-PERFORM

           DISPLAY series-term

           GOBACK
           .
