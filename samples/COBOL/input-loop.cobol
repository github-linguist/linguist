       IDENTIFICATION DIVISION.
       PROGRAM-ID. input-loop.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT in-stream ASSIGN TO KEYBOARD *> or any other file/stream
               ORGANIZATION LINE SEQUENTIAL
               FILE STATUS in-stream-status.

       DATA DIVISION.
       FILE SECTION.
       FD  in-stream.
       01  stream-line                 PIC X(80).

       WORKING-STORAGE SECTION.
       01  in-stream-status            PIC 99.
           88  end-of-stream           VALUE 10.

       PROCEDURE DIVISION.
           OPEN INPUT in-stream

           PERFORM UNTIL EXIT
               READ in-stream
                   AT END
                       EXIT PERFORM
               END-READ
               DISPLAY stream-line
           END-PERFORM

           CLOSE in-stream
           .
       END PROGRAM input-loop.
