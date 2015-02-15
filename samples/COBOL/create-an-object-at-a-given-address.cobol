       IDENTIFICATION DIVISION.
       PROGRAM-ID. object-address-test.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01 int-space.
          05 val PICTURE 9(5) VALUE 12345.
       01 addr BASED.
          05 val PICTURE 9(5) VALUE ZERO.
       01 point USAGE POINTER.
       PROCEDURE DIVISION.
         DISPLAY val OF int-space END-DISPLAY
         SET point TO ADDRESS OF int-space
         DISPLAY point END-DISPLAY
         SET ADDRESS OF addr TO point
         DISPLAY val OF addr END-DISPLAY
         MOVE 65535 TO val OF addr
         DISPLAY val OF addr END-DISPLAY
         DISPLAY val OF int-space END-DISPLAY
         STOP RUN.
       END PROGRAM object-address-test.
