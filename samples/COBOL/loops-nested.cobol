       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested-Loop.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       78  Table-Size VALUE 10.
       01  Table-Area.
           03  Table-Row OCCURS Table-Size TIMES
                   INDEXED BY Row-Index.
               05  Table-Element PIC 99 OCCURS Table-Size TIMES
                   INDEXED BY Col-Index.

       01  Current-Time PIC 9(8).
       PROCEDURE DIVISION.
*          *> Seed RANDOM.
           ACCEPT Current-Time FROM TIME
           MOVE FUNCTION RANDOM(Current-Time) TO Current-Time

*          *> Put random numbers in the table.
*          *> The AFTER clause is equivalent to a nested PERFORM VARYING
*          *> statement.
           PERFORM VARYING Row-Index FROM 1 BY 1
                       UNTIL Table-Size < Row-Index
                   AFTER Col-Index FROM 1 BY 1
                       UNTIL Table-Size < Col-Index
               COMPUTE Table-Element (Row-Index, Col-Index) =
                   FUNCTION MOD((FUNCTION RANDOM * 1000), 20) + 1
           END-PERFORM

*          *> Search through table for 20.
*          *> Using proper nested loops.
           PERFORM VARYING Row-Index FROM 1 BY 1
                   UNTIL Table-Size < Row-Index
               PERFORM VARYING Col-Index FROM 1 BY 1
                       UNTIL Table-Size < Col-Index
                   IF Table-Element (Row-Index, Col-Index) = 20
                       EXIT PERFORM
                   ELSE
                       DISPLAY Table-Element (Row-Index, Col-Index)
                   END-IF
               END-PERFORM
           END-PERFORM

           GOBACK
           .
