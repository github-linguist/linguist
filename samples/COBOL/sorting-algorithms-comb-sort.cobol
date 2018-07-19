       C-PROCESS SECTION.
       C-000.
           DISPLAY "SORT STARTING".

           MOVE WC-SIZE TO WC-GAP.

           PERFORM E-COMB UNTIL WC-GAP = 1 AND FINISHED.

           DISPLAY "SORT FINISHED".

       C-999.
           EXIT.

       E-COMB SECTION.
       E-000.
           IF WC-GAP > 1
              DIVIDE WC-GAP BY 1.3 GIVING WC-GAP
              IF WC-GAP = 9 OR 10
                 MOVE 11 TO WC-GAP.

           MOVE 1   TO WC-SUB-1.
           MOVE "Y" TO WF-FINISHED.

           PERFORM F-SCAN UNTIL WC-SUB-1 + WC-GAP > WC-SIZE.

       E-999.
           EXIT.

       F-SCAN SECTION.
       F-000.
           ADD WC-SUB-1 WC-GAP GIVING WC-SUB-2.
           IF WB-ENTRY(WC-SUB-1) > WB-ENTRY(WC-SUB-2)
              MOVE WB-ENTRY(WC-SUB-1) TO WC-TEMP
              MOVE WB-ENTRY(WC-SUB-2) TO WB-ENTRY(WC-SUB-1)
              MOVE WC-TEMP            TO WB-ENTRY(WC-SUB-2)
              MOVE "N"                TO WF-FINISHED.

           ADD 1 TO WC-SUB-1.

       F-999.
           EXIT.
