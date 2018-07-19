       >>SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. sedol.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT sedol-file ASSIGN "sedol.txt"
        ORGANIZATION LINE SEQUENTIAL
        FILE STATUS sedol-file-status.

DATA DIVISION.
FILE SECTION.
FD  sedol-file.
01  sedol                               PIC X(6).

WORKING-STORAGE SECTION.
01  sedol-file-status                   PIC XX.
    88  sedol-file-ok                   VALUE "00".

01  digit-num                           PIC 9 COMP.

01  digit-weights-area                  VALUE "1317391".
    03  digit-weights                   PIC 9 OCCURS 7 TIMES.

01  weighted-sum-parts-area.
    03  weighted-sum-parts              PIC 9(3) COMP OCCURS 6 TIMES.

01  weighted-sum                        PIC 9(3) COMP.

01  check-digit                         PIC 9.

PROCEDURE DIVISION.
    OPEN INPUT sedol-file
    PERFORM UNTIL NOT sedol-file-ok
        READ sedol-file
            AT END
                EXIT PERFORM
        END-READ

        MOVE FUNCTION UPPER-CASE(sedol) TO sedol

        PERFORM VARYING digit-num FROM 1 BY 1 UNTIL digit-num > 6
            EVALUATE TRUE
                WHEN sedol (digit-num:1) IS ALPHABETIC-UPPER
                    IF sedol (digit-num:1) = "A" OR "E" OR "I" OR "O" OR "U"
                        DISPLAY "Invalid SEDOL: " sedol
                        EXIT PERFORM CYCLE
                    END-IF

                    COMPUTE weighted-sum-parts (digit-num) =
                        (FUNCTION ORD(sedol (digit-num:1)) - FUNCTION ORD("A")
                        + 10) * digit-weights (digit-num)

                WHEN sedol (digit-num:1) IS NUMERIC
                    MULTIPLY FUNCTION NUMVAL(sedol (digit-num:1))
                        BY digit-weights (digit-num)
                        GIVING weighted-sum-parts (digit-num)

                WHEN OTHER
                    DISPLAY "Invalid SEDOL: " sedol
                    EXIT PERFORM CYCLE
            END-EVALUATE
        END-PERFORM

        INITIALIZE weighted-sum
        PERFORM VARYING digit-num FROM 1 BY 1 UNTIL digit-num > 6
            ADD weighted-sum-parts (digit-num) TO weighted-sum
        END-PERFORM

        COMPUTE check-digit =
            FUNCTION MOD(10 - FUNCTION MOD(weighted-sum, 10), 10)

        DISPLAY sedol check-digit
    END-PERFORM

    CLOSE sedol-file
    .
END PROGRAM sedol.
