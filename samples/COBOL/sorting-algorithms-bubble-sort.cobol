       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      BUBBLESORT.
       AUTHOR.                          DAVE STRATFORD.
       DATE-WRITTEN.                    MARCH 2010.
       INSTALLATION.                    HEXAGON SYSTEMS LIMITED.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                 ICL VME.
       OBJECT-COMPUTER.                 ICL VME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FA-INPUT-FILE  ASSIGN FL01.
           SELECT FB-OUTPUT-FILE ASSIGN FL02.

       DATA DIVISION.
       FILE SECTION.

       FD  FA-INPUT-FILE.
       01  FA-INPUT-REC.
         03  FA-DATA                    PIC S9(6).

       FD  FB-OUTPUT-FILE.
       01  FB-OUTPUT-REC                PIC S9(6).

       WORKING-STORAGE SECTION.
       01  WA-IDENTITY.
         03  WA-PROGNAME                PIC X(10) VALUE "BUBBLESORT".
         03  WA-VERSION                 PIC X(6) VALUE "000001".

       01  WB-TABLE.
         03  WB-ENTRY                   PIC 9(8) COMP SYNC OCCURS 100000
                                                  INDEXED BY WB-IX-1.

       01  WC-VARS.
         03  WC-SIZE                    PIC S9(8) COMP SYNC.
         03  WC-TEMP                    PIC S9(8) COMP SYNC.
         03  WC-END                     PIC S9(8) COMP SYNC.
         03  WC-LAST-CHANGE             PIC S9(8) COMP SYNC.

       01  WF-CONDITION-FLAGS.
         03  WF-EOF-FLAG                PIC X.
           88  END-OF-FILE              VALUE "Y".
         03  WF-EMPTY-FILE-FLAG         PIC X.
           88  EMPTY-FILE               VALUE "Y".

       PROCEDURE DIVISION.
       A-MAIN SECTION.
       A-000.
           PERFORM B-INITIALISE.
           IF NOT EMPTY-FILE
              PERFORM C-SORT.
           PERFORM D-FINISH.

       A-999.
           STOP RUN.

       B-INITIALISE SECTION.
       B-000.
           DISPLAY "*** " WA-PROGNAME " VERSION "
                          WA-VERSION " STARTING ***".

           MOVE ALL "N" TO WF-CONDITION-FLAGS.
           OPEN INPUT FA-INPUT-FILE.
           SET WB-IX-1 TO 0.

           READ FA-INPUT-FILE AT END MOVE "Y" TO WF-EOF-FLAG
                                                 WF-EMPTY-FILE-FLAG.

           PERFORM BA-READ-INPUT UNTIL END-OF-FILE.

           CLOSE FA-INPUT-FILE.

           SET WC-SIZE TO WB-IX-1.

       B-999.
           EXIT.

       BA-READ-INPUT SECTION.
       BA-000.
           SET WB-IX-1 UP BY 1.
           MOVE FA-DATA TO WB-ENTRY(WB-IX-1).

           READ FA-INPUT-FILE AT END MOVE "Y" TO WF-EOF-FLAG.

       BA-999.
           EXIT.

       C-SORT SECTION.
       C-000.
           DISPLAY "SORT STARTING".

           MOVE WC-SIZE TO WC-END.
           PERFORM E-BUBBLE UNTIL WC-END = 1.

           DISPLAY "SORT FINISHED".

       C-999.
           EXIT.

       D-FINISH SECTION.
       D-000.
           OPEN OUTPUT FB-OUTPUT-FILE.
           SET WB-IX-1 TO 1.

           PERFORM DA-WRITE-OUTPUT UNTIL WB-IX-1 > WC-SIZE.

           CLOSE FB-OUTPUT-FILE.

           DISPLAY "*** " WA-PROGNAME " FINISHED ***".

       D-999.
           EXIT.

       DA-WRITE-OUTPUT SECTION.
       DA-000.
           WRITE FB-OUTPUT-REC FROM WB-ENTRY(WB-IX-1).
           SET WB-IX-1 UP BY 1.

       DA-999.
           EXIT.

       E-BUBBLE SECTION.
       E-000.
           MOVE 1 TO WC-LAST-CHANGE.

           PERFORM F-PASS VARYING WB-IX-1 FROM 1 BY 1
                          UNTIL WB-IX-1 = WC-END.

           MOVE WC-LAST-CHANGE TO WC-END.

       E-999.
           EXIT.

       F-PASS SECTION.
       F-000.
           IF WB-ENTRY(WB-IX-1) > WB-ENTRY(WB-IX-1 + 1)
              SET  WC-LAST-CHANGE        TO WB-IX-1
              MOVE WB-ENTRY(WB-IX-1)     TO WC-TEMP
              MOVE WB-ENTRY(WB-IX-1 + 1) TO WB-ENTRY(WB-IX-1)
              MOVE WC-TEMP               TO WB-ENTRY(WB-IX-1 + 1).

       F-999.
           EXIT.
