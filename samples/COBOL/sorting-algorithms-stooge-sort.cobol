       >>SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. stooge-sort-test.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  Arr-Len                             CONSTANT 7.

01  arr-area                            VALUE "00004001000020000005000230000000000".
    03  arr-elt                         PIC 9(5) OCCURS Arr-Len TIMES
                                        INDEXED BY arr-idx.

PROCEDURE DIVISION.
    DISPLAY "Unsorted: " NO ADVANCING
    PERFORM VARYING arr-idx FROM 1 BY 1 UNTIL Arr-Len < arr-idx
        DISPLAY arr-elt (arr-idx) " " NO ADVANCING
    END-PERFORM
    DISPLAY SPACE

    CALL "stooge-sort" USING arr-area, OMITTED, OMITTED

    DISPLAY "Sorted:   " NO ADVANCING
    PERFORM VARYING arr-idx FROM 1 BY 1 UNTIL Arr-Len < arr-idx
        DISPLAY arr-elt (arr-idx) " " NO ADVANCING
    END-PERFORM
    DISPLAY SPACE
    .
END PROGRAM stooge-sort-test.


IDENTIFICATION DIVISION.
PROGRAM-ID. stooge-sort RECURSIVE.

DATA DIVISION.
LOCAL-STORAGE SECTION.
01  Arr-Len                             CONSTANT 7.

01  i                                   PIC 99 COMP.
01  j                                   PIC 99 COMP.

01  temp                                PIC 9(5).

01  t                                   PIC 99 COMP.

LINKAGE SECTION.
01  arr-area.
    03  arr-elt                         PIC 9(5) OCCURS Arr-Len TIMES.

01  i-val                               PIC 99 COMP.
01  j-val                               PIC 99 COMP.

PROCEDURE DIVISION USING arr-area, OPTIONAL i-val, OPTIONAL j-val.
    IF i-val IS OMITTED
        MOVE 1 TO i
    ELSE
        MOVE i-val TO i
    END-IF
    IF j-val IS OMITTED
        MOVE Arr-Len TO j
    ELSE
        MOVE j-val TO j
    END-IF

    IF arr-elt (j) < arr-elt (i)
        MOVE arr-elt (i) TO temp
        MOVE arr-elt (j) TO arr-elt (i)
        MOVE temp TO arr-elt (j)
    END-IF

    IF j - i + 1 >= 3
        COMPUTE t = (j - i + 1) / 3
        SUBTRACT t FROM j
        CALL "stooge-sort" USING arr-area, CONTENT i, j
        ADD t TO i, j
        CALL "stooge-sort" USING arr-area, CONTENT i, j
        SUBTRACT t FROM i, j
        CALL "stooge-sort" USING arr-area, CONTENT i, j
    END-IF
    .
END PROGRAM stooge-sort.
