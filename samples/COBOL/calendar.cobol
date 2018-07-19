       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CALEND.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  WS-DAY-NAMES-DEF.
         03 FILLER PIC X(09) VALUE 'SUNDAY   '.
         03 FILLER PIC X(09) VALUE 'MONDAY   '.
         03 FILLER PIC X(09) VALUE 'TUESDAY  '.
         03 FILLER PIC X(09) VALUE 'WEDNESDAY'.
         03 FILLER PIC X(09) VALUE 'THURSDAY '.
         03 FILLER PIC X(09) VALUE 'FRIDAY   '.
         03 FILLER PIC X(09) VALUE 'SATURDAY '.
       01  FILLER REDEFINES WS-DAY-NAMES-DEF.
         03  WS-DAY-NAME       PIC X(09) OCCURS 07 TIMES.

       01  WS-MTH-INFO-DEF.
         03 FILLER PIC X(11) VALUE 'JANUARY  31'.
         03 FILLER PIC X(11) VALUE 'FEBRUARY 28'.
         03 FILLER PIC X(11) VALUE 'MARCH    31'.
         03 FILLER PIC X(11) VALUE 'APRIL    30'.
         03 FILLER PIC X(11) VALUE 'MAY      31'.
         03 FILLER PIC X(11) VALUE 'JUNE     30'.
         03 FILLER PIC X(11) VALUE 'JULY     31'.
         03 FILLER PIC X(11) VALUE 'AUGUST   31'.
         03 FILLER PIC X(11) VALUE 'SEPTEMBER30'.
         03 FILLER PIC X(11) VALUE 'OCTOBER  31'.
         03 FILLER PIC X(11) VALUE 'NOVEMBER 30'.
         03 FILLER PIC X(11) VALUE 'DECEMBER 31'.
       01  FILLER REDEFINES WS-MTH-INFO-DEF.
         03  WS-MTH-INFO-TABLE OCCURS 12 TIMES.
           05  WS-MTH-INFO-NAME   PIC X(09).
           05  WS-MTH-INFO-DAYS   PIC 9(02).

       01  WS-MTH-AREA.
         03  WS-MTH-DD         PIC S99.
         03  WS-DAY1           PIC   9.
         03  WS-DAYS           PIC  99.
         03  WS-DD             PIC   9.
         03  WS-WK             PIC   9.
         03  WS-MM             PIC  99.
         03  WS-QQ             PIC  99.

         03  WS-MTH-MONTH  OCCURS 12 TIMES.
           05  WS-MTH-WEEK OCCURS 6 TIMES.
             07  WS-DAY-FLD      OCCURS 7 TIMES.
               09  WS-DAY        PIC ZZ.
       01  INPDATE-RECORD.
           05  INPD-YEAR          PIC 9(04).
           05  FILLER             PIC X(01).
           05  INPD-MONTH         PIC 9(02).
           05  FILLER             PIC X(01).
           05  INPD-DAY           PIC 9(02).
       01  WMS-DOW                PIC 9(01).
       01  WS-PRT                 PIC X(132).
       01  WS-COL                 PIC  9(03) VALUE 0.
       01  WS-PP                  PIC  9(03) VALUE 0.
       01  WS-CFGN.
         03  FILLER               PIC  9(03) VALUE  80.
         03  FILLER               PIC  9(02) VALUE  5.
         03  FILLER               PIC  9(01) VALUE  1.
         03  FILLER               PIC  9(02) VALUE  5.
         03  FILLER               PIC  9(01) VALUE  2.
       01  WS-CFGW.
         03  FILLER               PIC  9(03) VALUE 120.
         03  FILLER               PIC  9(02) VALUE 10.
         03  FILLER               PIC  9(01) VALUE  2.
         03  FILLER               PIC  9(02) VALUE 10.
         03  FILLER               PIC  9(01) VALUE  3.
       01  WS-CFG.
         03  WS-LS                PIC  9(03) VALUE 120.
         03  WS-LMAR              PIC  9(02) VALUE 10.
         03  WS-SPBD              PIC  9(01) VALUE  2.
         03  WS-SPBC              PIC  9(02) VALUE 10.
         03  WS-DNMW              PIC  9(01) VALUE  3.
       PROCEDURE DIVISION.
           MOVE '1969-01-01' TO INPDATE-RECORD
           MOVE WS-CFGN   TO WS-CFG
           IF  (FUNCTION MOD ( INPD-YEAR , 400 ) = 0
           OR  (FUNCTION MOD ( INPD-YEAR , 4   ) = 0
               AND
               FUNCTION MOD ( INPD-YEAR , 100 ) NOT = 0))
             MOVE 29         TO WS-MTH-INFO-DAYS (02)
           ELSE
             MOVE 28         TO WS-MTH-INFO-DAYS (02)
           END-IF

           PERFORM VARYING WS-MM FROM 1 BY +1
           UNTIL WS-MM > 12
           MOVE WS-MM TO INPD-MONTH
           CALL 'DATE2DOW' USING INPDATE-RECORD, WMS-DOW
           COMPUTE WS-MTH-DD = 1 - WMS-DOW
           COMPUTE WS-DAYS = WS-MTH-INFO-DAYS (INPD-MONTH)
           PERFORM VARYING WS-WK FROM 1 BY +1
           UNTIL WS-WK > 6
             PERFORM VARYING WS-DD FROM 1 BY +1
             UNTIL WS-DD > 7
               COMPUTE WS-MTH-DD = WS-MTH-DD + 1
               IF (WS-MTH-DD < 1)
               OR (WS-MTH-DD > WS-DAYS)
                 MOVE 0         TO WS-DAY (WS-MM, WS-WK, WS-DD)
               ELSE
                 MOVE WS-MTH-DD TO WS-DAY (WS-MM, WS-WK, WS-DD)
               END-IF
             END-PERFORM
           END-PERFORM
           END-PERFORM

           COMPUTE WS-MM = 0
           PERFORM VARYING WS-QQ FROM 1 BY +1
           UNTIL WS-QQ > 4

             INITIALIZE WS-PRT
             COMPUTE WS-PP = 1
             PERFORM VARYING WS-COL FROM 1 BY +1
             UNTIL WS-COL > 3
             COMPUTE WS-MM = 3 * (WS-QQ - 1) + WS-COL

               IF WS-COL = 1
                 COMPUTE WS-PP = WS-PP + WS-LMAR + 2 - WS-DNMW
               ELSE
                 COMPUTE WS-PP = WS-PP + WS-SPBC + 2 - WS-DNMW
               END-IF
               MOVE WS-MTH-INFO-NAME (WS-MM)
                             TO WS-PRT(WS-PP:9)
               COMPUTE WS-PP
               =       WS-PP + ( 2 * 7 + WS-SPBD * 6 + WS-SPBD - 1)
               -       4
               MOVE INPD-YEAR TO WS-PRT (WS-PP:4)
               COMPUTE WS-PP = WS-PP + 4
             END-PERFORM
             DISPLAY WS-PRT (1:WS-LS)

             INITIALIZE WS-PRT
             COMPUTE WS-PP = 1
             PERFORM VARYING WS-COL FROM 1 BY +1
             UNTIL WS-COL > 3
             COMPUTE WS-MM = 3 * (WS-QQ - 1) + WS-COL

               IF WS-COL = 1
                 COMPUTE WS-PP = WS-PP + WS-LMAR + 2 - WS-DNMW
               ELSE
                 COMPUTE WS-PP = WS-PP + WS-SPBC + 2 - WS-DNMW
               END-IF
               PERFORM VARYING WS-DD FROM 1 BY +1
               UNTIL WS-DD > 7
                 IF WS-DD > 1
                   COMPUTE WS-PP = WS-PP + WS-SPBD + 2 - WS-DNMW
                 END-IF
                 MOVE WS-DAY-NAME (WS-DD) (1:WS-DNMW)
                             TO WS-PRT (WS-PP:WS-DNMW)
                 COMPUTE WS-PP = WS-PP + WS-DNMW
               END-PERFORM
             END-PERFORM
             DISPLAY WS-PRT (1:WS-LS)

             PERFORM VARYING WS-WK FROM 1 BY +1
             UNTIL WS-WK > 6
               INITIALIZE WS-PRT
               COMPUTE WS-PP = 1
               PERFORM VARYING WS-COL FROM 1 BY +1
               UNTIL WS-COL > 3
               COMPUTE WS-MM = 3 * (WS-QQ - 1) + WS-COL

                 IF WS-COL = 1
                   COMPUTE WS-PP = WS-PP + WS-LMAR
                 ELSE
                   COMPUTE WS-PP = WS-PP + WS-SPBC
                 END-IF
                 PERFORM VARYING WS-DD FROM 1 BY +1
                 UNTIL WS-DD > 7
                   IF WS-DD > 1
                     COMPUTE WS-PP = WS-PP + WS-SPBD
                   END-IF
                   MOVE WS-DAY (WS-MM, WS-WK, WS-DD)
                               TO WS-PRT (WS-PP:2)
                   COMPUTE WS-PP = WS-PP + 2
                 END-PERFORM
               END-PERFORM
               DISPLAY WS-PRT (1:WS-LS)
             END-PERFORM
             DISPLAY ' '
           END-PERFORM
           GOBACK
           .
       END PROGRAM CALEND.
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  DATE2DOW.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WMS-WORK-AREA.
         03  WMS-YEAR       PIC 9(04).
         03  WMS-MONTH      PIC 9(02).
         03  WMS-CSYS       PIC 9(01) VALUE 1.
         03  WMS-SUM        pic 9(04).
       LINKAGE SECTION.
       01  INPDATE-RECORD.
           05  INPD-YEAR          PIC 9(04).
           05  FILLER             PIC X(01).
           05  INPD-MONTH         PIC 9(02).
           05  FILLER             PIC X(01).
           05  INPD-DAY           PIC 9(02).
       01  WMS-DOW                PIC 9(01).
       PROCEDURE DIVISION USING INPDATE-RECORD, WMS-DOW.
       1010-CONVERT-DATE-TO-DOW.
           IF INPD-MONTH < 3
               COMPUTE WMS-MONTH = INPD-MONTH + 12
               COMPUTE WMS-YEAR  = INPD-YEAR - 1
           ELSE
               COMPUTE WMS-MONTH = INPD-MONTH
               COMPUTE WMS-YEAR  = INPD-YEAR
           END-IF
           COMPUTE WMS-SUM  =
                            ( INPD-DAY + 2 * WMS-MONTH + WMS-YEAR
                            + FUNCTION INTEGER (6 * (WMS-MONTH + 1) / 10)
                            + FUNCTION INTEGER ( WMS-YEAR / 4   )
                            - FUNCTION INTEGER ( WMS-YEAR / 100 )
                            + FUNCTION INTEGER ( WMS-YEAR / 400 )
                            + WMS-CSYS )
           COMPUTE WMS-DOW = FUNCTION MOD (WMS-SUM, 7) + 1
           GOBACK
           .
       END PROGRAM DATE2DOW.
