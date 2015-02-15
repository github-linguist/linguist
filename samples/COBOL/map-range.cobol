       IDENTIFICATION DIVISION.
       PROGRAM-ID. demo-map-range.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  i                       USAGE FLOAT-LONG.

       01  mapped-num              USAGE FLOAT-LONG.

       01  a-begin                 USAGE FLOAT-LONG VALUE 0.
       01  a-end                   USAGE FLOAT-LONG VALUE 10.

       01  b-begin                 USAGE FLOAT-LONG VALUE -1.
       01  b-end                   USAGE FLOAT-LONG VALUE 0.

       01  i-display               PIC --9.9.
       01  mapped-display          PIC --9.9.

       PROCEDURE DIVISION.
           PERFORM VARYING i FROM 0 BY 1 UNTIL i > 10
               CALL "map-range" USING CONTENT a-begin, a-end, b-begin,
                   b-end, i, REFERENCE mapped-num
               COMPUTE i-display ROUNDED = i
               COMPUTE mapped-display ROUNDED = mapped-num
               DISPLAY FUNCTION TRIM(i-display) " maps to "
                   FUNCTION TRIM(mapped-display)
           END-PERFORM
           .
       END PROGRAM demo-map-range.


       IDENTIFICATION DIVISION.
       PROGRAM-ID. map-range.

       DATA DIVISION.
       LINKAGE SECTION.
       01  a-begin                 USAGE FLOAT-LONG.
       01  a-end                   USAGE FLOAT-LONG.

       01  b-begin                 USAGE FLOAT-LONG.
       01  b-end                   USAGE FLOAT-LONG.

       01  val-to-map              USAGE FLOAT-LONG.

       01  ret                     USAGE FLOAT-LONG.

       PROCEDURE DIVISION USING a-begin, a-end, b-begin, b-end,
               val-to-map, ret.
           COMPUTE ret =
               b-begin + ((val-to-map - a-begin) * (b-end - b-begin)
                   / (a-end - a-begin))
           .
       END PROGRAM map-range.
