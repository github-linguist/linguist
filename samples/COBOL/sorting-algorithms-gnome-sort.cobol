       C-SORT SECTION.
       C-000.
           DISPLAY "SORT STARTING".

           SET WB-IX-1 TO 2.
           MOVE 1 TO WC-NEXT-POSN.

           PERFORM E-GNOME UNTIL WC-NEXT-POSN > WC-SIZE.

           DISPLAY "SORT FINISHED".

       C-999.
           EXIT.

       E-GNOME SECTION.
       E-000.
           IF WB-ENTRY(WB-IX-1 - 1) NOT > WB-ENTRY(WB-IX-1)
              ADD 1       TO WC-NEXT-POSN
              SET WB-IX-1 TO WC-NEXT-POSN
           ELSE
              MOVE WB-ENTRY(WB-IX-1 - 1) TO WC-TEMP
              MOVE WB-ENTRY(WB-IX-1)     TO WB-ENTRY(WB-IX-1 - 1)
              MOVE WC-TEMP               TO WB-ENTRY(WB-IX-1)
              SET WB-IX-1                DOWN BY 1
              IF WB-IX-1 = 1
                 ADD 1       TO WC-NEXT-POSN
                 SET WB-IX-1 TO WC-NEXT-POSN.

       E-999.
           EXIT.
