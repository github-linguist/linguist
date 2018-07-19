       IDENTIFICATION DIVISION.
       PROGRAM-ID. knuth-shuffle.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  i                       PIC 9(8).
       01  j                       PIC 9(8).

       01  temp                    PIC 9(8).

       LINKAGE SECTION.
       78  Table-Len               VALUE 10.
       01  ttable-area.
           03  ttable              PIC 9(8) OCCURS Table-Len TIMES.

       PROCEDURE DIVISION USING ttable-area.
           MOVE FUNCTION RANDOM(FUNCTION CURRENT-DATE (11:6)) TO i

           PERFORM VARYING i FROM Table-Len BY -1 UNTIL i = 0
               COMPUTE j =
                   FUNCTION MOD(FUNCTION RANDOM * 10000, Table-Len) + 1

               MOVE ttable (i) TO temp
               MOVE ttable (j) TO ttable (i)
               MOVE temp TO ttable (j)
           END-PERFORM

           GOBACK
           .
