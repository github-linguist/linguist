       IDENTIFICATION DIVISION.
       PROGRAM-ID. NOTES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL notes ASSIGN TO "NOTES.TXT"
               ORGANIZATION LINE SEQUENTIAL
               FILE STATUS note-status.

       DATA DIVISION.
       FILE SECTION.
       FD  notes.
       01  note-record       PIC X(256).

       LOCAL-STORAGE SECTION.
       01  note-status       PIC 99.
           88  notes-ok      VALUE 0 THRU 9.

       01  date-now.
           03  current-year  PIC 9(4).
           03  current-month PIC 99.
           03  current-day   PIC 99.

       01  time-now.
           03  current-hour  PIC 99.
           03  current-min   PIC 99.
           03  current-sec   PIC 99.

       01  args              PIC X(256).

       PROCEDURE DIVISION.
       DECLARATIVES.
       note-error SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON notes.

           DISPLAY "Error using NOTES.TXT. Error code: " note-status
           .
       END DECLARATIVES.

       main.
           ACCEPT args FROM COMMAND-LINE

*          *> If there are no args, display contents of NOTES.TXT.
           IF args = SPACES
               OPEN INPUT notes

               PERFORM FOREVER
*                  *> READ has no syntax highlighting, but END-READ does.
*                  *> Go figure.
                   READ notes
                       AT END
                           EXIT PERFORM

                       NOT AT END
                           DISPLAY FUNCTION TRIM(note-record)
                   END-READ
               END-PERFORM
           ELSE
               OPEN EXTEND notes

*              *> Write date and time to file.
               ACCEPT date-now FROM DATE YYYYMMDD
               ACCEPT time-now FROM TIME
               STRING current-year "-" current-month "-" current-day
                   " " current-hour ":" current-min ":" current-sec
                   INTO note-record
               WRITE note-record

*              *> Write arguments to file as they were passed.
               STRING X"09", args INTO note-record
               WRITE note-record
           END-IF

           CLOSE notes

           GOBACK
           .
