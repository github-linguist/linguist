       IDENTIFICATION DIVISION.
       PROGRAM-ID. 100Doors.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Current-n      PIC 9(3).
       01 StepSize       PIC 9(3).
       01 DoorTable.
          02 Doors       PIC 9(1)   OCCURS 100 TIMES.
             88 ClosedDoor          VALUE ZERO.
       01 Idx            PIC 9(3).

       PROCEDURE DIVISION.
       Begin.
           INITIALIZE DoorTable
           PERFORM VARYING StepSize FROM 1 BY 1 UNTIL StepSize > 100
             PERFORM VARYING Current-n FROM StepSize BY StepSize
                     UNTIL Current-n > 100
               SUBTRACT Doors (Current-n) FROM 1 GIVING Doors (Current-n)
             END-PERFORM
           END-PERFORM

           PERFORM VARYING Idx FROM 1 BY 1
                   UNTIL Idx > 100
             IF ClosedDoor (Idx)
               DISPLAY Idx " is closed."
             ELSE
               DISPLAY Idx " is open."
             END-IF
           END-PERFORM

           STOP RUN
           .
