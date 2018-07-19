       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      MERGESORT.
       AUTHOR.                          DAVE STRATFORD.
       DATE-WRITTEN.                    APRIL 2010.
       INSTALLATION.                    HEXAGON SYSTEMS LIMITED.
      ******************************************************************
      *                            MERGE SORT                          *
      *  The Merge sort uses a completely different paradigm, one of   *
      * divide and conquer, to many of the other sorts. The data set   *
      * is split into smaller sub sets upon which are sorted and then  *
      * merged together to form the final sorted data set.             *
      *  This version uses the recursive method. Split the data set in *
      * half and perform a merge sort on each half. This in turn splits*
      * each half again and again until each set is just one or 2 items*
      * long. A set of one item is already sorted so is ignored, a set *
      * of two is compared and swapped as necessary. The smaller data  *
      * sets are then repeatedly merged together to eventually form the*
      * full, sorted, set.                                             *
      *  Since cobol cannot do recursion this module only simulates it *
      * so is not as fast as a normal recursive version would be.      *
      *  Scales very well to larger data sets, its relative complexity *
      * means it is not suited to sorting smaller data sets: use an    *
      * Insertion sort instead as the Merge sort is a stable sort.     *
      ******************************************************************

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
         03  FA-DATA                    PIC 9(6).

       FD  FB-OUTPUT-FILE.
       01  FB-OUTPUT-REC                PIC 9(6).

       WORKING-STORAGE SECTION.
       01  WA-IDENTITY.
         03  WA-PROGNAME                PIC X(10) VALUE "MERGESORT".
         03  WA-VERSION                 PIC X(6) VALUE "000001".

       01  WB-TABLE.
         03  WB-ENTRY                   PIC 9(8) COMP SYNC OCCURS 100000
                                                 INDEXED BY WB-IX-1
                                                            WB-IX-2.

       01  WC-VARS.
         03  WC-SIZE                    PIC S9(8) COMP SYNC.
         03  WC-TEMP                    PIC S9(8) COMP SYNC.
         03  WC-START                   PIC S9(8) COMP SYNC.
         03  WC-MIDDLE                  PIC S9(8) COMP SYNC.
         03  WC-END                     PIC S9(8) COMP SYNC.

       01  WD-FIRST-HALF.
         03  WD-FH-MAX                  PIC S9(8) COMP SYNC.
         03  WD-ENTRY                   PIC 9(8) COMP SYNC OCCURS 50000
                                                 INDEXED BY WD-IX.

       01  WF-CONDITION-FLAGS.
         03  WF-EOF-FLAG                PIC X.
           88  END-OF-FILE              VALUE "Y".
         03  WF-EMPTY-FILE-FLAG         PIC X.
           88  EMPTY-FILE               VALUE "Y".

       01  WS-STACK.
      * This stack is big enough to sort a list of 1million items.
         03  WS-STACK-ENTRY OCCURS 20 INDEXED BY WS-STACK-TOP.
           05  WS-START                 PIC S9(8) COMP SYNC.
           05  WS-MIDDLE                PIC S9(8) COMP SYNC.
           05  WS-END                   PIC S9(8) COMP SYNC.
           05  WS-FS-FLAG               PIC X.
             88  FIRST-HALF             VALUE "F".
             88  SECOND-HALF            VALUE "S".
             88  WS-ALL                 VALUE "A".
           05  WS-IO-FLAG               PIC X.
             88  WS-IN                  VALUE "I".
             88  WS-OUT                 VALUE "O".

       PROCEDURE DIVISION.
       A-MAIN SECTION.
       A-000.
           PERFORM B-INITIALISE.

           IF NOT EMPTY-FILE
              PERFORM C-PROCESS.

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

       C-PROCESS SECTION.
       C-000.
           DISPLAY "SORT STARTING".

           MOVE 1           TO WS-START(1).
           MOVE WC-SIZE     TO WS-END(1).
           MOVE "F"         TO WS-FS-FLAG(1).
           MOVE "I"         TO WS-IO-FLAG(1).
           SET WS-STACK-TOP TO 2.

           PERFORM E-MERGE-SORT UNTIL WS-OUT(1).

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

      ******************************************************************
       E-MERGE-SORT SECTION.
      *=====================                                           *
      * This section controls the simulated recursion.                 *
      ******************************************************************
       E-000.
           IF WS-OUT(WS-STACK-TOP - 1)
              GO TO E-010.

           MOVE WS-START(WS-STACK-TOP - 1) TO WC-START.
           MOVE WS-END(WS-STACK-TOP - 1)   TO WC-END.

      * First check size of part we are dealing with.
           IF WC-END - WC-START = 0
      * Only 1 number in range, so simply set for output, and move on
              MOVE "O" TO WS-IO-FLAG(WS-STACK-TOP - 1)
              GO TO E-010.

           IF WC-END - WC-START = 1
      * 2 numbers, so compare and swap as necessary. Set for output
              MOVE "O" TO WS-IO-FLAG(WS-STACK-TOP - 1)
              IF WB-ENTRY(WC-START) > WB-ENTRY(WC-END)
                 MOVE WB-ENTRY(WC-START) TO WC-TEMP
                 MOVE WB-ENTRY(WC-END) TO WB-ENTRY(WC-START)
                 MOVE WC-TEMP TO WB-ENTRY(WC-END)
                 GO TO E-010
              ELSE
                 GO TO E-010.

      * More than 2, so split and carry on down
           COMPUTE WC-MIDDLE = ( WC-START + WC-END ) / 2.

           MOVE WC-START  TO WS-START(WS-STACK-TOP).
           MOVE WC-MIDDLE TO WS-END(WS-STACK-TOP).
           MOVE "F"       TO WS-FS-FLAG(WS-STACK-TOP).
           MOVE "I"       TO WS-IO-FLAG(WS-STACK-TOP).
           SET WS-STACK-TOP UP BY 1.

           GO TO E-999.

       E-010.
           SET WS-STACK-TOP DOWN BY 1.

           IF SECOND-HALF(WS-STACK-TOP)
              GO TO E-020.

           MOVE WS-START(WS-STACK-TOP - 1) TO WC-START.
           MOVE WS-END(WS-STACK-TOP - 1)   TO WC-END.
           COMPUTE WC-MIDDLE = ( WC-START + WC-END ) / 2 + 1.

           MOVE WC-MIDDLE TO WS-START(WS-STACK-TOP).
           MOVE WC-END    TO WS-END(WS-STACK-TOP).
           MOVE "S"       TO WS-FS-FLAG(WS-STACK-TOP).
           MOVE "I"       TO WS-IO-FLAG(WS-STACK-TOP).
           SET WS-STACK-TOP UP BY 1.

           GO TO E-999.

       E-020.
           MOVE WS-START(WS-STACK-TOP - 1) TO WC-START.
           MOVE WS-END(WS-STACK-TOP - 1)   TO WC-END.
           COMPUTE WC-MIDDLE = ( WC-START + WC-END ) / 2.
           PERFORM H-PROCESS-MERGE.
           MOVE "O" TO WS-IO-FLAG(WS-STACK-TOP - 1).

       E-999.
           EXIT.

      ******************************************************************
       H-PROCESS-MERGE SECTION.
      *========================                                        *
      * This section identifies which data is to be merged, and then   *
      * merges the two data streams into a single larger data stream.  *
      ******************************************************************
       H-000.
           INITIALISE WD-FIRST-HALF.
           COMPUTE WD-FH-MAX = WC-MIDDLE - WC-START + 1.
           SET WD-IX                        TO 1.

           PERFORM HA-COPY-OUT VARYING WB-IX-1 FROM WC-START BY 1
                               UNTIL WB-IX-1 > WC-MIDDLE.

           SET WB-IX-1 TO WC-START.
           SET WB-IX-2 TO WC-MIDDLE.
           SET WB-IX-2 UP BY 1.
           SET WD-IX   TO 1.

           PERFORM HB-MERGE UNTIL WD-IX > WD-FH-MAX OR WB-IX-2 > WC-END.

           PERFORM HC-COPY-BACK UNTIL WD-IX > WD-FH-MAX.

       H-999.
           EXIT.

       HA-COPY-OUT SECTION.
       HA-000.
           MOVE WB-ENTRY(WB-IX-1) TO WD-ENTRY(WD-IX).
           SET WD-IX UP BY 1.

       HA-999.
           EXIT.

       HB-MERGE SECTION.
       HB-000.
           IF WB-ENTRY(WB-IX-2) < WD-ENTRY(WD-IX)
              MOVE WB-ENTRY(WB-IX-2) TO WB-ENTRY(WB-IX-1)
              SET WB-IX-2            UP BY 1
           ELSE
              MOVE WD-ENTRY(WD-IX) TO WB-ENTRY(WB-IX-1)
              SET WD-IX            UP BY 1.

           SET WB-IX-1 UP BY 1.

       HB-999.
           EXIT.

       HC-COPY-BACK SECTION.
       HC-000.
           MOVE WD-ENTRY(WD-IX) TO WB-ENTRY(WB-IX-1).
           SET WD-IX            UP BY 1.
           SET WB-IX-1          UP BY 1.

       HC-999.
           EXIT.
