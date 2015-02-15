       IDENTIFICATION DIVISION.
       PROGRAM-ID. Date-Format.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  Days-Area.
           03  Days-Data.
               05  FILLER PIC X(9) VALUE "Monday".
               05  FILLER PIC X(9) VALUE "Tuesday".
               05  FILLER PIC X(9) VALUE "Wednesday".
               05  FILLER PIC X(9) VALUE "Thursday".
               05  FILLER PIC X(9) VALUE "Friday".
               05  FILLER PIC X(9) VALUE "Saturday".
               05  FILLER PIC X(9) VALUE "Sunday".

           03  Days-Values REDEFINES Days-Data.
               05  Days-Table PIC X(9) OCCURS 7 TIMES.

       01  Months-Area.
           03  Months-Data.
               05  FILLER PIC X(9) VALUE "January".
               05  FILLER PIC X(9) VALUE "February".
               05  FILLER PIC X(9) VALUE "March".
               05  FILLER PIC X(9) VALUE "April".
               05  FILLER PIC X(9) VALUE "May".
               05  FILLER PIC X(9) VALUE "June".
               05  FILLER PIC X(9) VALUE "July".
               05  FILLER PIC X(9) VALUE "August".
               05  FILLER PIC X(9) VALUE "September".
               05  FILLER PIC X(9) VALUE "October".
               05  FILLER PIC X(9) VALUE "November".
               05  FILLER PIC X(9) VALUE "December".

           03  Months-Values REDEFINES Months-Data.
               05  Months-Table PIC X(9) OCCURS 12 TIMES.

       01  Current-Date-Str.
           03  Current-Year     PIC X(4).
           03  Current-Month    PIC X(2).
           03  Current-Day      PIC X(2).

       01  Current-Day-Of-Week  PIC 9.

       PROCEDURE DIVISION.
           MOVE FUNCTION CURRENT-DATE (1:8) TO Current-Date-Str

           DISPLAY Current-Year "-" Current-Month "-" Current-Day

           ACCEPT Current-Day-Of-Week FROM DAY-OF-WEEK
           DISPLAY
               FUNCTION TRIM(
                   Days-Table (FUNCTION NUMVAL(Current-Day-Of-Week)))
               ", "
               FUNCTION TRIM(
                   Months-Table (FUNCTION NUMVAL(Current-Month)))
               " "
               Current-Day
               ", "
               Current-Year
           END-DISPLAY

           GOBACK
           .
