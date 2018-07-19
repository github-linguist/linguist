           PERFORM E-SELECTION VARYING WB-IX-1 FROM 1 BY 1
                               UNTIL WB-IX-1 = WC-SIZE.

...

       E-SELECTION SECTION.
       E-000.
           SET WC-LOWEST   TO WB-IX-1.
           ADD 1 WC-LOWEST GIVING WC-START

           PERFORM F-PASS VARYING WB-IX-2 FROM WC-START BY 1
                          UNTIL WB-IX-2 > WC-SIZE.

           IF WB-IX-1 NOT = WC-LOWEST
              MOVE WB-ENTRY(WC-LOWEST) TO WC-TEMP
              MOVE WB-ENTRY(WB-IX-1)   TO WB-ENTRY(WC-LOWEST)
              MOVE WC-TEMP             TO WB-ENTRY(WB-IX-1).

       E-999.
           EXIT.

       F-PASS SECTION.
       F-000.
           IF WB-ENTRY(WB-IX-2) < WB-ENTRY(WC-LOWEST)
              SET WC-LOWEST TO WB-IX-2.

       F-999.
           EXIT.
