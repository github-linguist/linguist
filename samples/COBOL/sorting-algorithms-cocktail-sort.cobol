       C-SORT SECTION.
       C-000.
           DISPLAY "SORT STARTING".

           MOVE 2       TO WC-START
           MOVE WC-SIZE TO WC-END.
           MOVE 1       TO WC-DIRECTION
                           WC-LAST-CHANGE.
           PERFORM E-SHAKER UNTIL WC-END * WC-DIRECTION <
                                  WC-START * WC-DIRECTION.

           DISPLAY "SORT FINISHED".

       C-999.
           EXIT.

       E-SHAKER SECTION.
       E-000.
           PERFORM F-PASS VARYING WB-IX-1 FROM WC-START BY WC-DIRECTION
                          UNTIL WB-IX-1 = WC-END + WC-DIRECTION.

           MOVE WC-START TO WC-END.
           SUBTRACT WC-DIRECTION FROM WC-LAST-CHANGE GIVING WC-START.
           MULTIPLY WC-DIRECTION BY -1 GIVING WC-DIRECTION.

       E-999.
           EXIT.

       F-PASS SECTION.
       F-000.
           IF WB-ENTRY(WB-IX-1 - 1) > WB-ENTRY(WB-IX-1)
              SET  WC-LAST-CHANGE        TO WB-IX-1
              MOVE WB-ENTRY(WB-IX-1 - 1) TO WC-TEMP
              MOVE WB-ENTRY(WB-IX-1)     TO WB-ENTRY(WB-IX-1 - 1)
              MOVE WC-TEMP               TO WB-ENTRY(WB-IX-1).

       F-999.
           EXIT.
