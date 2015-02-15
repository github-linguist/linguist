       IDENTIFICATION DIVISION.
       PROGRAM-ID. langtons-ant.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  Grid-Size               VALUE 100.
       01  grid-area.
           03  grid-x              OCCURS Grid-Size TIMES.
               05  grid-y          OCCURS Grid-Size TIMES.
                   07  cell-colour PIC X VALUE "W".
                       88  black   VALUE "B".
                       88  white   VALUE "W".

       01  ant-x                   PIC 999.
       01  ant-y                   PIC 999.

       01  ant-direction           PIC 9.
           88  upward              VALUE 0.
           88  rightward           VALUE 1.
           88  downward            VALUE 2.
           88  leftward            VALUE 3.

       78  Pause-Time-Ns           VALUE 10000000.

       01  display-y               PIC 999.

       78  Black-Background        VALUE 0.
       78  White-Background        VALUE 7.

       01  i                       PIC 999.
       01  j                       PIC 999.

       01  pause                   PIC X.

       PROCEDURE DIVISION.
       main-line.
           DIVIDE Grid-Size BY 2 GIVING ant-x, ant-y

           PERFORM display-initial-grid
           PERFORM UNTIL (ant-x = Grid-Size OR 0)
                   OR (ant-y = Grid-Size OR 0)
               PERFORM step-simulation
               CALL "CBL_OC_NANOSLEEP" USING Pause-Time-Ns
           END-PERFORM

           DISPLAY "Press enter to quit." AT LINE 1 COLUMN 1
           ACCEPT pause

           GOBACK
           .
       step-simulation.
           IF black (ant-x, ant-y)
               SET white (ant-x, ant-y) TO TRUE
               PERFORM display-ant-cell
               COMPUTE ant-direction =
                   FUNCTION MOD(ant-direction + 1, 4)
           ELSE
               SET black (ant-x, ant-y) TO TRUE
               PERFORM display-ant-cell
               COMPUTE ant-direction =
                   FUNCTION MOD(ant-direction - 1, 4)
           END-IF

           EVALUATE TRUE
               WHEN upward
                   ADD 1 TO ant-y
               WHEN rightward
                   ADD 1 TO ant-x
               WHEN downward
                   SUBTRACT 1 FROM ant-y
               WHEN leftward
                   SUBTRACT 1 FROM ant-x
           END-EVALUATE
           .
       display-ant-cell.
               SUBTRACT ant-y FROM Grid-Size GIVING display-y
               IF black (ant-x, ant-y)
                   DISPLAY SPACE AT LINE display-y COLUMN ant-x
                       WITH BACKGROUND-COLOR Black-Background
               ELSE
                   DISPLAY SPACE AT LINE display-y COLUMN ant-x
                      WITH BACKGROUND-COLOR White-Background
               END-IF
               .
       display-initial-grid.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > Grid-Size
                   AFTER j FROM 1 BY 1 UNTIL j > Grid-Size
               DISPLAY SPACE AT LINE i COLUMN j
                   WITH BACKGROUND-COLOR White-Background
           END-PERFORM
           .
