       >>SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. towers-of-hanoi.

PROCEDURE DIVISION.
    CALL "move-disk" USING 4, 1, 2, 3
    .
END PROGRAM towers-of-hanoi.

IDENTIFICATION DIVISION.
PROGRAM-ID. move-disk RECURSIVE.

DATA DIVISION.
LINKAGE SECTION.
01  n                         PIC 9 USAGE COMP.
01  from-pole                 PIC 9 USAGE COMP.
01  to-pole                   PIC 9 USAGE COMP.
01  via-pole                  PIC 9 USAGE COMP.

PROCEDURE DIVISION USING n, from-pole, to-pole, via-pole.
    IF n > 0
       SUBTRACT 1 FROM n
       CALL "move-disk" USING CONTENT n, from-pole, via-pole, to-pole
       DISPLAY "Move disk from pole " from-pole " to pole " to-pole
       CALL "move-disk" USING CONTENT n, via-pole, to-pole, from-pole
    END-IF
    .
END PROGRAM move-disk.
