      ******************************************************************
      * This COBOL program generates and analyzes the Fibonacci sequence 
      * with comprehensive statistical calculations. 
      * It classifies each  number by parity (even/odd), primality, and 
      * size category while tracking sequence properties. 
      *
      * The program calculates mathematical statistics including sum, 
      * average, maximum, minimum values, and percentage distributions. 
      * It displays formatted results with detailed analysis including 
      * each number's ratio to its predecessor. 
      *
      * Finally, it presents a complete statistical summary 
      * demonstrating advanced COBOL data processing and reporting 
      * capabilities.
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADVFIBB.
       AUTHOR. Antonio Gibas.
       DATE-WRITTEN. 07-12-2025.
       DATE-COMPILED. 07-12-2025.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z.
       OBJECT-COMPUTER. IBM-Z.
       SPECIAL-NAMES.
           CURRENCY SIGN IS '$'
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  PROGRAM-CONTROLS.
           05  WS-MAX-ELEMENTS   PIC 9(3) VALUE 50.
           05  WS-ACTUAL-COUNT   PIC 9(3).
           05  WS-RETURN-CODE    PIC 9(4).
           05  WS-INDICATORS.
               10  WS-VALID-DATA PIC X VALUE 'Y'.
                   88  DATA-IS-VALID    VALUE 'Y'.
                   88  DATA-INVALID     VALUE 'N'.
               10  WS-EOF        PIC X VALUE 'N'.
                   88  END-OF-PROCESS   VALUE 'Y'.
           
       01  FIBONACCI-TABLE.
           05  FIB-ENTRY OCCURS 1 TO 50 TIMES
                        DEPENDING ON WS-ACTUAL-COUNT.
               10  FIB-NUMBER    PIC 9(12).
               10  FIB-STATUS    PIC X.
                   88  IS-EVEN         VALUE 'E'.
                   88  IS-ODD          VALUE 'O'.
                   88  IS-PRIME        VALUE 'P'.
               10  FIB-CATEGORY  PIC 9.
                   88  SMALL-NUMBER    VALUE 1.
                   88  MEDIUM-NUMBER   VALUE 2.
                   88  LARGE-NUMBER    VALUE 3.
           
       01  MATHEMATICAL-OPERATIONS.
           05  WS-TEMP-AREA      PIC 9(12).
           05  WS-SUM            PIC 9(12) VALUE ZERO.
           05  WS-AVERAGE        PIC 9(9)V9(2).
           05  WS-MAX-VALUE      PIC 9(12) VALUE ZERO.
           05  WS-MIN-VALUE      PIC 9(12) VALUE ZERO.
           
       01  STATISTICAL-DATA.
           05  STAT-COUNT-EVEN   PIC 9(3) VALUE ZERO.
           05  STAT-COUNT-ODD    PIC 9(3) VALUE ZERO.
           05  STAT-COUNT-PRIME  PIC 9(3) VALUE ZERO.
           05  STAT-PERCENTAGES.
               10  PCT-EVEN      PIC 9(3)V9(2).
               10  PCT-ODD       PIC 9(3)V9(2).
               10  PCT-PRIME     PIC 9(3)V9(2).
           
       01  FORMATTED-OUTPUT.
           05  HEADER-LINE.
               10  FILLER        PIC X(20) VALUE 
                   'FIBONACCI ANALYSIS'.
               10  FILLER        PIC X(20) VALUE 
                   'ENTERPRISE COBOL'.
               10  TIMESTAMP     PIC X(8).
           
           05  DETAIL-LINE.
               10  DL-INDEX      PIC ZZ9.
               10  FILLER        PIC X(5) VALUE ' | '.
               10  DL-VALUE      PIC Z(11)9.
               10  FILLER        PIC X(5) VALUE ' | '.
               10  DL-TYPE       PIC X(10).
               10  FILLER        PIC X(5) VALUE ' | '.
               10  DL-RATIO      PIC Z9.9(6).
           
           05  SUMMARY-LINE.
               10  FILLER        PIC X(15) VALUE 
                   'TOTAL NUMBERS:'.
               10  SL-COUNT      PIC ZZZ9.
               10  FILLER        PIC X(10) VALUE 
                   ' | EVEN: '.
               10  SL-EVEN       PIC ZZ9.
               10  FILLER        PIC X(10) VALUE 
                   ' | ODD: '.
               10  SL-ODD        PIC ZZ9.
               10  FILLER        PIC X(12) VALUE 
                   ' | PRIME: '.
               10  SL-PRIME      PIC ZZ9.
           
           05  STATS-LINE.
               10  FILLER        PIC X(10) VALUE 'AVERAGE: '.
               10  SL-AVG        PIC Z(9)9.99.
               10  FILLER        PIC X(12) VALUE ' | MAX: '.
               10  SL-MAX        PIC Z(11)9.
               10  FILLER        PIC X(12) VALUE ' | MIN: '.
               10  SL-MIN        PIC Z(11)9.
           
       01  TIME-DATA.
           05  WS-CURRENT-DATE.
               10  WS-YEAR       PIC 9(4).
               10  WS-MONTH      PIC 9(2).
               10  WS-DAY        PIC 9(2).
           05  WS-CURRENT-TIME.
               10  WS-HOUR       PIC 9(2).
               10  WS-MINUTE     PIC 9(2).
               10  WS-SECOND     PIC 9(2).
               10  WS-HUNDREDTH  PIC 9(2).
           
       01  CURRENT-DATE-TIME.
           05  CD-YEAR          PIC 9(4).
           05  CD-MONTH         PIC 9(2).
           05  CD-DAY           PIC 9(2).
           05  CD-HOUR          PIC 9(2).
           05  CD-MINUTE        PIC 9(2).
           05  CD-SECOND        PIC 9(2).
           05  CD-HUNDREDTH     PIC 9(2).
           05  CD-GMT-DIFF      PIC S9(4).
           05  CD-GMT-HOUR      PIC 9(2).
           05  CD-GMT-MINUTE    PIC 9(2).
           
       77  WS-PRIME-CHECK        PIC 9(12).
       77  WS-DIVISOR            PIC 9(12).
       77  WS-MOD-RESULT         PIC 9(12).
       77  WS-IS-PRIME           PIC X VALUE 'Y'.
       77  WS-INDEX              PIC 9(3).
       77  WS-INDEX-1            PIC 9(3).
       77  WS-INDEX-2            PIC 9(3).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           
           PERFORM 000-INITIALIZE
           PERFORM 100-GENERATE-FIBONACCI
           PERFORM 200-ANALYZE-SEQUENCE
           PERFORM 300-CALCULATE-STATISTICS
           PERFORM 400-DISPLAY-RESULTS
           PERFORM 900-TERMINATE
           
           GOBACK.
       
       000-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-TIME
           MOVE CD-YEAR  TO WS-YEAR
           MOVE CD-MONTH TO WS-MONTH
           MOVE CD-DAY   TO WS-DAY
           MOVE CD-HOUR  TO WS-HOUR
           MOVE CD-MINUTE TO WS-MINUTE
           MOVE CD-SECOND TO WS-SECOND
           
           DISPLAY '======================================='
           DISPLAY '   ENTERPRISE COBOL FIBONACCI ANALYZER'
           DISPLAY '======================================='
           DISPLAY 'Date: ' WS-YEAR '/' WS-MONTH '/' WS-DAY
           DISPLAY 'Time: ' WS-HOUR ':' WS-MINUTE ':' WS-SECOND
           DISPLAY SPACE
           
           SET DATA-IS-VALID TO TRUE
           MOVE ZERO TO WS-ACTUAL-COUNT.
       
       100-GENERATE-FIBONACCI.
           DISPLAY 'GENERATING FIBONACCI SEQUENCE...'
           DISPLAY SPACE
           
           IF WS-MAX-ELEMENTS > 50
               DISPLAY 'WARNING: MAX ELEMENTS LIMITED TO 50'
               MOVE 50 TO WS-ACTUAL-COUNT
           ELSE
               MOVE WS-MAX-ELEMENTS TO WS-ACTUAL-COUNT
           END-IF
           
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                     UNTIL WS-INDEX > WS-ACTUAL-COUNT
               
               EVALUATE TRUE
                   WHEN WS-INDEX = 1
                       MOVE 0 TO FIB-NUMBER(WS-INDEX)
                   WHEN WS-INDEX = 2
                       MOVE 1 TO FIB-NUMBER(WS-INDEX)
                   WHEN OTHER
                       COMPUTE WS-INDEX-1 = WS-INDEX - 1
                       COMPUTE WS-INDEX-2 = WS-INDEX - 2
                       COMPUTE FIB-NUMBER(WS-INDEX) =
                           FIB-NUMBER(WS-INDEX-1) +
                           FIB-NUMBER(WS-INDEX-2)
               END-EVALUATE
               
               PERFORM 110-CLASSIFY-NUMBER
               
           END-PERFORM.
       
       110-CLASSIFY-NUMBER.
           DIVIDE FIB-NUMBER(WS-INDEX) BY 2
               GIVING WS-TEMP-AREA
               REMAINDER WS-MOD-RESULT
           
           IF WS-MOD-RESULT = 0
               SET IS-EVEN(WS-INDEX) TO TRUE
           ELSE
               SET IS-ODD(WS-INDEX) TO TRUE
           END-IF
           
           PERFORM 120-CHECK-PRIME
           
           EVALUATE TRUE
               WHEN FIB-NUMBER(WS-INDEX) < 1000
                   SET SMALL-NUMBER(WS-INDEX) TO TRUE
               WHEN FIB-NUMBER(WS-INDEX) < 1000000
                   SET MEDIUM-NUMBER(WS-INDEX) TO TRUE
               WHEN OTHER
                   SET LARGE-NUMBER(WS-INDEX) TO TRUE
           END-EVALUATE.
       
       120-CHECK-PRIME.
           MOVE 'Y' TO WS-IS-PRIME
           
           IF FIB-NUMBER(WS-INDEX) < 2
               MOVE 'N' TO WS-IS-PRIME
           ELSE
               PERFORM VARYING WS-DIVISOR FROM 2 BY 1
                         UNTIL WS-DIVISOR * WS-DIVISOR 
                               > FIB-NUMBER(WS-INDEX)
                         OR WS-IS-PRIME = 'N'
                   
                   DIVIDE FIB-NUMBER(WS-INDEX) BY WS-DIVISOR
                       GIVING WS-TEMP-AREA
                       REMAINDER WS-MOD-RESULT
                   
                   IF WS-MOD-RESULT = 0
                       MOVE 'N' TO WS-IS-PRIME
                   END-IF
                   
               END-PERFORM
           END-IF
           
           IF WS-IS-PRIME = 'Y'
               SET IS-PRIME(WS-INDEX) TO TRUE
           END-IF.
       
       200-ANALYZE-SEQUENCE.
           DISPLAY 'ANALYZING SEQUENCE PROPERTIES...'
           DISPLAY SPACE
           
           MOVE ZERO TO WS-SUM
           MOVE ZERO TO STAT-COUNT-EVEN
           MOVE ZERO TO STAT-COUNT-ODD
           MOVE ZERO TO STAT-COUNT-PRIME
           
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                     UNTIL WS-INDEX > WS-ACTUAL-COUNT
               
               ADD FIB-NUMBER(WS-INDEX) TO WS-SUM
               
               IF IS-EVEN(WS-INDEX)
                   ADD 1 TO STAT-COUNT-EVEN
               END-IF
               
               IF IS-ODD(WS-INDEX)
                   ADD 1 TO STAT-COUNT-ODD
               END-IF
               
               IF IS-PRIME(WS-INDEX)
                   ADD 1 TO STAT-COUNT-PRIME
               END-IF
               
           END-PERFORM.
       
       300-CALCULATE-STATISTICS.
           COMPUTE WS-AVERAGE ROUNDED = 
               WS-SUM / WS-ACTUAL-COUNT
           
           MOVE FIB-NUMBER(1) TO WS-MAX-VALUE
           MOVE FIB-NUMBER(1) TO WS-MIN-VALUE
           
           PERFORM VARYING WS-INDEX FROM 2 BY 1
                     UNTIL WS-INDEX > WS-ACTUAL-COUNT
               
               IF FIB-NUMBER(WS-INDEX) > WS-MAX-VALUE
                   MOVE FIB-NUMBER(WS-INDEX) 
                     TO WS-MAX-VALUE
               END-IF
               
               IF FIB-NUMBER(WS-INDEX) < WS-MIN-VALUE
                   MOVE FIB-NUMBER(WS-INDEX) 
                     TO WS-MIN-VALUE
               END-IF
               
           END-PERFORM
           
           COMPUTE PCT-EVEN ROUNDED = 
               (STAT-COUNT-EVEN * 100) / WS-ACTUAL-COUNT
           
           COMPUTE PCT-ODD ROUNDED = 
               (STAT-COUNT-ODD * 100) / WS-ACTUAL-COUNT
           
           COMPUTE PCT-PRIME ROUNDED = 
               (STAT-COUNT-PRIME * 100) / WS-ACTUAL-COUNT.
       
       400-DISPLAY-RESULTS.
           DISPLAY 'FIBONACCI SEQUENCE RESULTS:'
           DISPLAY '==========================='
           DISPLAY SPACE
           
           DISPLAY 'IDX |      VALUE      |   TYPE   |   RATIO'
           DISPLAY '----+-----------------+----------+-----------'
           
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                     UNTIL WS-INDEX > WS-ACTUAL-COUNT
               
               MOVE WS-INDEX TO DL-INDEX
               MOVE FIB-NUMBER(WS-INDEX) TO DL-VALUE
               
               EVALUATE TRUE
                   WHEN IS-EVEN(WS-INDEX) AND IS-PRIME(WS-INDEX)
                       MOVE 'EVEN PRIME' TO DL-TYPE
                   WHEN IS-EVEN(WS-INDEX)
                       MOVE 'EVEN     ' TO DL-TYPE
                   WHEN IS-ODD(WS-INDEX) AND IS-PRIME(WS-INDEX)
                       MOVE 'ODD PRIME' TO DL-TYPE
                   WHEN OTHER
                       MOVE 'ODD      ' TO DL-TYPE
               END-EVALUATE
               
               IF WS-INDEX > 1
                   COMPUTE WS-INDEX-1 = WS-INDEX - 1
                   IF FIB-NUMBER(WS-INDEX-1) NOT = 0
                       COMPUTE DL-RATIO ROUNDED = 
                           FIB-NUMBER(WS-INDEX) / 
                           FIB-NUMBER(WS-INDEX-1)
                   ELSE
                       MOVE 0 TO DL-RATIO
                   END-IF
               ELSE
                   MOVE 0 TO DL-RATIO
               END-IF
               
               DISPLAY DETAIL-LINE
               
           END-PERFORM
           
           DISPLAY SPACE
           DISPLAY SPACE
           
           MOVE WS-ACTUAL-COUNT TO SL-COUNT
           MOVE STAT-COUNT-EVEN TO SL-EVEN
           MOVE STAT-COUNT-ODD TO SL-ODD
           MOVE STAT-COUNT-PRIME TO SL-PRIME
           
           DISPLAY SUMMARY-LINE
           DISPLAY SPACE
           
           MOVE WS-AVERAGE TO SL-AVG
           MOVE WS-MAX-VALUE TO SL-MAX
           MOVE WS-MIN-VALUE TO SL-MIN
           
           DISPLAY STATS-LINE
           DISPLAY SPACE
           
           DISPLAY 'PERCENTAGE DISTRIBUTION:'
           DISPLAY '  Even numbers:  ' PCT-EVEN '%'
           DISPLAY '  Odd numbers:   ' PCT-ODD '%'
           DISPLAY '  Prime numbers: ' PCT-PRIME '%'.
       
       900-TERMINATE.
           DISPLAY SPACE
           DISPLAY '======================================='
           DISPLAY '   ANALYSIS COMPLETED SUCCESSFULLY'
           DISPLAY '======================================='
           
           MOVE 0 TO WS-RETURN-CODE
           SET END-OF-PROCESS TO TRUE.
