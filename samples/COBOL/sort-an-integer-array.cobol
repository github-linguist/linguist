       PROGRAM-ID. sort-ints.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  array-area             VALUE "54321".
           03  array              PIC 9 OCCURS 5 TIMES.
       01  i                      PIC 9.

       PROCEDURE DIVISION.
       main-line.
           PERFORM display-array
           SORT array ASCENDING array
           PERFORM display-array

           GOBACK
           .
       display-array.
           PERFORM VARYING i FROM 1 BY 1 UNTIL 5 < i
               DISPLAY array (i) " " NO ADVANCING
           END-PERFORM
           DISPLAY SPACE
           .
