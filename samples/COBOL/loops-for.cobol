       IDENTIFICATION DIVISION.
       PROGRAM-ID. Display-Triangle.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Outer-Counter PIC 9.
       01  Inner-Counter PIC 9.

       PROCEDURE DIVISION.
       PERFORM VARYING Outer-Counter FROM 1 BY 1 UNTIL 5 < Outer-Counter

           PERFORM VARYING Inner-Counter FROM 1 BY 1
                   UNTIL Outer-Counter < Inner-Counter
               DISPLAY "*" NO ADVANCING
           END-PERFORM

           DISPLAY "" *> Output a newline
       END-PERFORM

       GOBACK
       .
