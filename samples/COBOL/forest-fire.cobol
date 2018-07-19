       IDENTIFICATION DIVISION.
       PROGRAM-ID. forest-fire.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       *> Probability represents a fraction of 10000.
       *> For instance, IGNITE-PROB means a tree has a 1 in 10000 chance
       *> of igniting.
       78  IGNITE-PROB                 VALUE 1.
       78  NEW-TREE-PROB               VALUE 100.

       78  EMPTY-PROB                  VALUE 3333.

       78  AREA-SIZE                   VALUE 40.

       01  sim-table.
           03  sim-row OCCURS AREA-SIZE TIMES INDEXED BY row-index.
               05  sim-area OCCURS AREA-SIZE TIMES
                   INDEXED BY col-index.
                   07  current-status  PIC 9.
                       *> The flags correspond to the colours they will
                       *> be displayed as.
                       88  empty       VALUE 0. *> Black
                       88  tree        VALUE 2. *> Green
                       88  burning     VALUE 4. *> Red

                   07  next-status     PIC 9.
                       88  empty       VALUE 0.
                       88  tree        VALUE 2.
                       88  burning     VALUE 4.

       01  rand-num                    PIC 9999.

       01  next-row                    PIC 9(4).
       01  next-col                    PIC 9(4).

       01  neighbours-row              PIC 9(4).
       01  neighbours-col              PIC 9(4).

       PROCEDURE DIVISION.
       main-line.
           *> Seed RANDOM with current time.
           MOVE FUNCTION RANDOM(FUNCTION CURRENT-DATE (9:8)) TO rand-num

           PERFORM initialise-table
           PERFORM FOREVER
               PERFORM show-simulation
               PERFORM step-simulation
           END-PERFORM

           GOBACK
           .

       initialise-table.
           PERFORM VARYING row-index FROM 1 BY 1
                   UNTIL AREA-SIZE < row-index
                   AFTER col-index FROM 1 BY 1
                       UNTIL AREA-SIZE < col-index
               PERFORM get-rand-num
               IF rand-num <= EMPTY-PROB
                   SET empty OF current-status (row-index, col-index)
                       TO TRUE
                   SET empty OF next-status (row-index, col-index)
                       TO TRUE
               ELSE
                   SET tree OF current-status (row-index, col-index)
                       TO TRUE
                   SET tree OF next-status (row-index, col-index)
                       TO TRUE
               END-IF
           END-PERFORM
           .

       show-simulation.
           PERFORM VARYING row-index FROM 1 BY 1
                   UNTIL AREA-SIZE < row-index
                   AFTER col-index FROM 1 BY 1
                       UNTIL AREA-SIZE < col-index
                DISPLAY SPACE AT LINE row-index COLUMN col-index
                    WITH BACKGROUND-COLOR
                        current-status (row-index, col-index)
           END-PERFORM
           .

       *> Updates the simulation.
       step-simulation.
            PERFORM VARYING row-index FROM 1 BY 1
                   UNTIL AREA-SIZE < row-index
                   AFTER col-index FROM 1 BY 1
                       UNTIL AREA-SIZE < col-index
                EVALUATE TRUE
                    WHEN empty OF current-status (row-index, col-index)
                        PERFORM get-rand-num
                        IF rand-num <= NEW-TREE-PROB
                            SET tree OF next-status
                                 (row-index, col-index) TO TRUE
                        END-IF

                    WHEN tree OF current-status (row-index, col-index)
                        PERFORM simulate-tree

                    WHEN burning OF current-status
                            (row-index, col-index)
                        SET empty OF next-status (row-index, col-index)
                            TO TRUE
                END-EVALUATE
            END-PERFORM

            PERFORM update-statuses.
            .

       *> Updates a tree tile, assuming row-index and col-index are at
       *> a tree area.
       simulate-tree.
           *> Find the row and column of the bottom-right neighbour.
           COMPUTE next-row = FUNCTION MIN(row-index + 1, AREA-SIZE)
           COMPUTE next-col = FUNCTION MIN(col-index + 1, AREA-SIZE)

           COMPUTE neighbours-row = FUNCTION MAX(row-index - 1, 1)
           COMPUTE neighbours-col = FUNCTION MAX(col-index - 1, 1)

           *> If a neighbour is burning, catch fire.
           PERFORM VARYING neighbours-row FROM neighbours-row BY 1
                   UNTIL next-row < neighbours-row
               *> Check if neighbours in a row are on fire.
               PERFORM VARYING neighbours-col FROM neighbours-col BY 1
                       UNTIL next-col < neighbours-col
                   IF neighbours-row = row-index
                           AND neighbours-col = col-index
                       EXIT PERFORM CYCLE
                   END-IF

                   IF burning OF current-status
                           (neighbours-row, neighbours-col)
                       SET burning OF next-status (row-index, col-index)
                           TO TRUE
                       EXIT PARAGRAPH
                   END-IF
               END-PERFORM

               *> Move neighbours-col back to starting position
               COMPUTE neighbours-col =
                   FUNCTION MAX(neighbours-col - 3, 1)
           END-PERFORM

           *> Otherwise, there is a random chance of
           *> catching fire.
           PERFORM get-rand-num
           IF rand-num <= IGNITE-PROB
               SET burning OF next-status (row-index, col-index) TO TRUE
           END-IF
           .

       update-statuses.
           PERFORM VARYING row-index FROM 1 BY 1
                   UNTIL AREA-SIZE < row-index
                   AFTER col-index FROM 1 BY 1
                       UNTIL AREA-SIZE < col-index
               MOVE next-status (row-index, col-index)
                   TO current-status (row-index, col-index)
           END-PERFORM
           .

       *> Puts a random value between 0 and 9999 in rand-num.
       get-rand-num.
           COMPUTE rand-num =
               FUNCTION MOD(FUNCTION RANDOM * 100000, 10000)
           .
