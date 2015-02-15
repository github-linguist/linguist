       >>SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. count-in-octal.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
    FUNCTION dec-to-oct
    .
DATA DIVISION.
WORKING-STORAGE SECTION.
01  i                                   PIC 9(18).

PROCEDURE DIVISION.
    PERFORM VARYING i FROM 1 BY 1 UNTIL i = 0
        DISPLAY FUNCTION dec-to-oct(i)
    END-PERFORM
    .
END PROGRAM count-in-octal.


IDENTIFICATION DIVISION.
FUNCTION-ID. dec-to-oct.

DATA DIVISION.
LOCAL-STORAGE SECTION.
01  rem                                 PIC 9.

01  dec                                 PIC 9(18).

LINKAGE SECTION.
01  dec-arg                             PIC 9(18).

01  oct                                 PIC 9(18).

PROCEDURE DIVISION USING dec-arg RETURNING oct.
    MOVE dec-arg TO dec *> Copy is made to avoid modifying reference arg.
    PERFORM WITH TEST AFTER UNTIL dec = 0
        MOVE FUNCTION REM(dec, 8) TO rem
        STRING rem, oct DELIMITED BY SPACES INTO oct
        DIVIDE 8 INTO dec
    END-PERFORM
    .
END FUNCTION dec-to-oct.
