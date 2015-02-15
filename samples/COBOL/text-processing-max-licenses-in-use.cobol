       IDENTIFICATION DIVISION.
       PROGRAM-ID. max-licenses-in-use.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT license-file ASSIGN "mlijobs.txt"
               ORGANIZATION LINE SEQUENTIAL
               FILE STATUS file-status.

       DATA DIVISION.
       FILE SECTION.
       FD  license-file.
       01  license-record.
           03  FILLER                   PIC X(8).
           03  action                   PIC X(3).
               88  license-out          VALUE "OUT".
           03  FILLER                   PIC X(3).
           03  license-timestamp        PIC X(19).
           03  FILLER                   PIC X(13).

       WORKING-STORAGE SECTION.
       01  file-status                  PIC XX.
           88  file-ok                  VALUE "00".

       01  max-licenses-out             PIC 9(6).
       01  num-max-times                PIC 99.
       01  max-license-times-area.
           03  max-timestamps           PIC X(19) OCCURS 1 TO 50 TIMES
               DEPENDING ON num-max-times.
       01  current-licenses-out         PIC 9(6).

       01  i                            PIC 99.

       PROCEDURE DIVISION.
       DECLARATIVES.
       license-file-error SECTION.
           USE AFTER ERROR ON license-file.

           DISPLAY "An unexpected error has occurred. Error "
               file-status ". The program will close."
           GOBACK
           .
       END DECLARATIVES.

       main-line.
           OPEN INPUT license-file
           IF NOT file-ok
               DISPLAY "File could not be opened. Error " file-status
                   "."
               GOBACK
           END-IF

           PERFORM FOREVER
               READ license-file
                   AT END
                       EXIT PERFORM
               END-READ

               IF license-out
                   ADD 1 TO current-licenses-out

                   EVALUATE TRUE
                       WHEN current-licenses-out > max-licenses-out
                           MOVE 1 TO num-max-times
                           MOVE current-licenses-out TO max-licenses-out
                           MOVE license-timestamp
                               TO max-timestamps (num-max-times)

                       WHEN current-licenses-out = max-licenses-out
                           ADD 1 TO num-max-times
                           MOVE license-timestamp
                               TO max-timestamps (num-max-times)
                   END-EVALUATE
               ELSE
                   SUBTRACT 1 FROM current-licenses-out
               END-IF
           END-PERFORM

           CLOSE license-file

           DISPLAY "License count at log end: " current-licenses-out
           DISPLAY "Maximum simulataneous licenses: " max-licenses-out
           DISPLAY "Time(s):"
           PERFORM VARYING i FROM 1 BY 1 UNTIL num-max-times < i
               DISPLAY max-timestamps (i)
           END-PERFORM

           GOBACK
           .
