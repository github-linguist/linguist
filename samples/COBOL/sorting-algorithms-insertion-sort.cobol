       C-PROCESS SECTION.
           PERFORM E-INSERTION VARYING WB-IX-1 FROM 1 BY 1
                               UNTIL WB-IX-1 > WC-SIZE.

...

       E-INSERTION SECTION.
       E-000.
           MOVE WB-ENTRY(WB-IX-1) TO WC-TEMP.
           SET WB-IX-2 TO WB-IX-1.

           PERFORM F-PASS UNTIL WB-IX-2 NOT > 1 OR
                                WC-TEMP NOT < WB-ENTRY(WB-IX-2 - 1).

           IF WB-IX-1 NOT = WB-IX-2
              MOVE WC-TEMP TO WB-ENTRY(WB-IX-2).

       E-999.
           EXIT.

       F-PASS SECTION.
       F-000.
           MOVE WB-ENTRY(WB-IX-2 - 1) TO WB-ENTRY(WB-IX-2).
           SET WB-IX-2                DOWN BY 1.

       F-999.
           EXIT.
