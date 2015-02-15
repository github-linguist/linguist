       IDENTIFICATION DIVISION.
       PROGRAM-ID. terminal-dimensions.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  num-lines PIC 9(3).
       01  num-cols  PIC 9(3).

       SCREEN SECTION.
       01  display-screen.
           03  LINE 01 COL 01 PIC 9(3) FROM num-lines.
           03  LINE 01 COL 05 VALUE "rows by " .
           03  LINE 01 COL 13 PIC 9(3) FROM num-cols.
           03  LINE 01 COL 16 VALUE " columns.".

       PROCEDURE DIVISION.
           ACCEPT num-lines FROM LINES
           ACCEPT num-cols FROM COLUMNS

           DISPLAY display-screen

      *    This pauses the program, as ncurses will immediately revert
      *    back to the console when the program ends.
           CALL "C$SLEEP" USING BY CONTENT 3

           GOBACK
           .
