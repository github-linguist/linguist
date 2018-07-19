       >>SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. random-element.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  nums-area                           VALUE "123456789".
    03  nums                            PIC 9 OCCURS 9 TIMES.

01  random-idx                          PIC 9 COMP.

PROCEDURE DIVISION.
    COMPUTE random-idx = FUNCTION RANDOM(FUNCTION CURRENT-DATE (9:7)) * 9 + 1
    DISPLAY nums (random-idx)
    .
END PROGRAM random-element.
