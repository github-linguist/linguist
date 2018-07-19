       IDENTIFICATION DIVISION.
       PROGRAM-ID. Exec-Hq9.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       78  Code-Length VALUE 256.

       01  i           PIC 999.
       01  accumulator PIC 999.

       01  bottles     PIC 999.

       LINKAGE SECTION.
       01  hq9-code    PIC X(Code-Length).

       PROCEDURE DIVISION USING BY VALUE hq9-code.
           PERFORM VARYING i FROM 1 BY 1 UNTIL Code-Length < i
               EVALUATE hq9-code (i:1)
                   WHEN "Q"
                       DISPLAY FUNCTION TRIM(hq9-code)

                   WHEN "H"
                       DISPLAY "Hello, World!"

                   WHEN "9"
                       MOVE 99 TO bottles
                       PERFORM UNTIL bottles = ZERO
                           DISPLAY
                               bottles " bottles of beer on the wall"
                           DISPLAY bottles " bottles of beer"
                           DISPLAY "Take one down, pass it around"
                           SUBTRACT 1 FROM bottles
                           DISPLAY
                               bottles " bottles of beer on the wall"
                           DISPLAY SPACE
                       END-PERFORM

                   WHEN "+"
                       ADD 1 TO accumulator
               END-EVALUATE
           END-PERFORM

           GOBACK
           .
