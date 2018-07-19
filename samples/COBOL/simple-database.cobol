       IDENTIFICATION DIVISION.
       PROGRAM-ID. simple-database.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL database-file ASSIGN Database-Path
               ORGANIZATION INDEXED
               ACCESS SEQUENTIAL
               RECORD KEY data-title
               ALTERNATE RECORD KEY data-tag
                   WITH DUPLICATES
               ALTERNATE RECORD KEY date-added
                   WITH DUPLICATES
               FILE STATUS file-status
               .
       DATA DIVISION.
       FILE SECTION.
       FD  database-file.
       01  database-record.
           *> Date is in YYYYMMDD format.
           03  date-added          PIC 9(8).
           03  data-tag            PIC X(20).
           03  data-title          PIC X(50).
           03  data-contents       PIC X(200).
           *> Adding extra space is considered good style so the record
           *> can be expanded in the future.
           03  FILLER              PIC X(50).

       WORKING-STORAGE SECTION.
       78  Database-Path           VALUE "database.dat".

       01  file-status             PIC XX.
           88  file-ok             VALUE "00".
           88  duplicate-key       VALUE "22".
           88  key-not-found       VALUE "23".

       01  num-args                PIC 99.

       01  action                  PIC XX.
           88  create-entry        VALUE "-c".
           88  remove-entry        VALUE "-r".
           88  find-entry          VALUE "-f".
           88  print-latest        VALUE "-l".
           88  print-database      VALUES "-a", "-d", "-t".
           *> Printed by title.
           88  print-by-title      VALUE "-a".
           88  print-by-date       VALUE "-d".
           88  print-by-tag        VALUE "-t".
           88  print-help          VALUES "-h", SPACES.

       01  read-direction-flag     PIC X VALUE SPACE.
           88  read-backwards      VALUE "B".

       01  edited-date             PIC 9(4)/99/99.
       PROCEDURE DIVISION.
       DECLARATIVES.
       database-file-error SECTION.
           USE AFTER ERROR ON database-file

           DISPLAY "An error has occurred while using " Database-Path
               ". Error no. " file-status
           DISPLAY "The program will terminate."

           CLOSE database-file

           GOBACK
           .
       END DECLARATIVES.

       main-line.
           DISPLAY 1 UPON ARGUMENT-NUMBER
           ACCEPT action FROM ARGUMENT-VALUE

           ACCEPT num-args FROM ARGUMENT-NUMBER

           EVALUATE TRUE
               WHEN create-entry
                   IF num-args >= 4
                       PERFORM write-entry
                   ELSE
                       DISPLAY "-a requires arguments to enter in the "
                           "database. See help (-h) for details."
                   END-IF

               WHEN remove-entry
                   IF num-args >= 2
                       PERFORM delete-entry
                   ELSE
                       DISPLAY "-r requires the title of the entry to "
                           "delete."
                   END-IF

               WHEN find-entry
                   IF num-args >= 2
                       PERFORM display-specified-entry
                   ELSE
                       DISPLAY "-f requires the title of the entry to "
                           "find."
                   END-IF

               WHEN print-latest
                   PERFORM show-latest

               WHEN print-database
                   PERFORM show-database

               WHEN print-help
                   PERFORM show-general-help

               WHEN OTHER
                   DISPLAY action " is not a valid option."
           END-EVALUATE

           GOBACK
           .
       write-entry.
           OPEN EXTEND database-file

           DISPLAY 2 UPON ARGUMENT-NUMBER
           ACCEPT data-tag FROM ARGUMENT-VALUE
           DISPLAY 3 UPON ARGUMENT-NUMBER
           ACCEPT data-title FROM ARGUMENT-VALUE
           IF data-title = SPACES
               DISPLAY "The title cannot be blank."
               PERFORM close-and-terminate
           END-IF

           DISPLAY 4 UPON ARGUMENT-NUMBER
           ACCEPT data-contents FROM ARGUMENT-VALUE

           ACCEPT date-added FROM DATE YYYYMMDD

           WRITE database-record
               INVALID KEY
                   IF duplicate-key
                       DISPLAY "An entry in the database already has "
                           "that title. Please choose a different "
                           "title or remove the entry."
                   ELSE
                       PERFORM database-file-error
                   END-IF
           END-WRITE

           PERFORM close-database
           .
       delete-entry.
           PERFORM get-title-arg
           OPEN I-O database-file
           PERFORM read-title

           DELETE database-file

           PERFORM close-database
           .
       display-specified-entry.
           PERFORM get-title-arg
           OPEN INPUT database-file
           PERFORM read-title

           PERFORM show-record

           PERFORM close-database
           .
       get-title-arg.
           DISPLAY 2 UPON ARGUMENT-NUMBER
           ACCEPT data-title FROM ARGUMENT-VALUE
           .
       read-title.
           START database-file KEY IS = data-title
               INVALID KEY
                   IF key-not-found
                       DISPLAY "An entry with that title was not found."
                       PERFORM close-and-terminate
                   ELSE
                       PERFORM database-file-error
                   END-IF
           END-START

           READ database-file
           .
       close-and-terminate.
            PERFORM close-database
            GOBACK
            .
       show-latest.
           OPEN INPUT database-file

           PERFORM start-at-last-date
           READ database-file
           PERFORM show-record

           PERFORM close-database
           .
       show-database.
           OPEN INPUT database-file

           EVALUATE TRUE
               WHEN print-by-title
                   *> Primary key is the title.
                   CONTINUE
               WHEN print-by-tag
                   MOVE LOW-VALUES TO data-tag
                   START database-file KEY IS > data-tag
               WHEN print-by-date
                   PERFORM start-at-last-date
                   SET read-backwards TO TRUE
           END-EVALUATE

           PERFORM FOREVER
               *> The problem with statements instead of functions...
               IF NOT read-backwards
                   READ database-file NEXT
                       AT END
                           EXIT PERFORM
                   END-READ
               ELSE
                   READ database-file PREVIOUS
                       AT END
                           EXIT PERFORM
                   END-READ
               END-IF

               PERFORM show-record
               DISPLAY SPACE
           END-PERFORM

           PERFORM close-database
           .
       start-at-last-date.
           MOVE HIGH-VALUES TO date-added
           START database-file KEY IS < date-added
           .
       close-database.
           CLOSE database-file
           .
       show-record.
           MOVE date-added TO edited-date
           DISPLAY "Date added: " edited-date " Tag: " data-tag
           DISPLAY "Title: " data-title
           DISPLAY "Contents:"
           DISPLAY "  " FUNCTION TRIM(data-contents)
           .
       show-general-help.
           DISPLAY "Help: Possible options are:"
           DISPLAY "  -a - Show all the entries (sorted by title)."
           DISPLAY "  -c - Create a new entry in the database. -c needs"
               " further arguments in this format:"
           DISPLAY '    "tag" "title" "content"'
           DISPLAY "    Max argument sizes (in characters): tag - 20, "
               "title - 50, content - 200"
           DISPLAY "    The title must be unique and not be blank."
           DISPLAY "  -d - Show all the entries sorted by date added."
           DISPLAY "  -f - Finds and displays entry with the title "
               "provided. The title should be specified as shown for "
               "-c."
           DISPLAY "  -h - Show this help menu."
           DISPLAY "  -l - Show the latest entry."
           DISPLAY "  -r - Remove the entry with the title provided. "
               "The title should be specified as shown for -c."
           DISPLAY "  -t - Show all the entries sorted by tag."
           .
