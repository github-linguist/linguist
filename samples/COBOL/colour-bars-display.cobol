       IDENTIFICATION DIVISION.
       PROGRAM-ID. terminal-colour-bars.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  width           PIC 9(3).
       01  height          PIC 9(3).

       01  interval        PIC 9(3).

       01  colours-area.
           03  colour-values.
               05  FILLER  PIC 9 VALUE 0. *> Black
               05  FILLER  PIC 9 VALUE 4. *> Red
               05  FILLER  PIC 9 VALUE 2. *> Green
               05  FILLER  PIC 9 VALUE 1. *> Blue
               05  FILLER  PIC 9 VALUE 5. *> Magneta
               05  FILLER  PIC 9 VALUE 3. *> Cyan
               05  FILLER  PIC 9 VALUE 6. *> Yellow
               05  FILLER  PIC 9 VALUE 7. *> White

           03  colour-table REDEFINES colour-values.
               05  colours PIC 9 OCCURS 8 TIMES INDEXED BY colour-index.

       01  i               PIC 9(3).
       01  j               PIC 9(3).

       PROCEDURE DIVISION.
           ACCEPT width FROM COLUMNS
           ACCEPT height FROM LINES
           DIVIDE width BY 8 GIVING interval

           PERFORM VARYING i FROM 1 BY 1 UNTIL height < i
               PERFORM VARYING j FROM 1 BY 1 UNTIL width < j
                  COMPUTE colour-index = (j / interval) + 1

                  IF 8 < colour-index
                      SET colour-index TO 8
                  END-IF

                  *> Some colours come a bit darker than they
                  *> should, with the yellow being orange and the white
                  *> being light-grey.
                  DISPLAY SPACE AT LINE i COLUMN j
                      WITH BACKGROUND-COLOR colours (colour-index)
               END-PERFORM
           END-PERFORM

           ACCEPT i *> Prevent ncurses returning to console immediately.

           GOBACK
           .
