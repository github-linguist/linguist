       >>SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. run-length-encoding.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
    FUNCTION encode
    FUNCTION decode
    .
DATA DIVISION.
WORKING-STORAGE SECTION.
01  input-str                           PIC A(100).
01  encoded                             PIC X(200).
01  decoded                             PIC X(200).

PROCEDURE DIVISION.
    ACCEPT input-str
    MOVE encode(FUNCTION TRIM(input-str)) TO encoded
    DISPLAY "Encoded: " FUNCTION TRIM(encoded)
    DISPLAY "Decoded: " FUNCTION TRIM(decode(encoded))
    .
END PROGRAM run-length-encoding.


IDENTIFICATION DIVISION.
FUNCTION-ID. encode.

DATA DIVISION.
LOCAL-STORAGE SECTION.
01  str-len                             PIC 9(3) COMP.

01  i                                   PIC 9(3) COMP.

01  current-char                        PIC A.

01  num-chars                           PIC 9(3) COMP.
01  num-chars-disp                      PIC Z(3).

01  encoded-pos                         PIC 9(3) COMP VALUE 1.

LINKAGE SECTION.
01  str                                 PIC X ANY LENGTH.

01  encoded                             PIC X(200).

PROCEDURE DIVISION USING str RETURNING encoded.
    MOVE FUNCTION LENGTH(str) TO str-len
    MOVE str (1:1) TO current-char
    MOVE 1 TO num-chars
    PERFORM VARYING i FROM 2 BY 1 UNTIL i > str-len
        IF str (i:1) <> current-char
            CALL "add-num-chars" USING encoded, encoded-pos,
                CONTENT current-char, num-chars

            MOVE str (i:1) TO current-char
            MOVE 1 TO num-chars
        ELSE
            ADD 1 TO num-chars
        END-IF
    END-PERFORM

    CALL "add-num-chars" USING encoded, encoded-pos, CONTENT current-char,
        num-chars
    .
END FUNCTION encode.

IDENTIFICATION DIVISION.
PROGRAM-ID. add-num-chars.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  num-chars-disp                      PIC Z(3).

LINKAGE SECTION.
01  str                                 PIC X(200).

01  current-pos                         PIC 9(3) COMP.

01  char-to-encode                      PIC X.

01  num-chars                           PIC 9(3) COMP.

PROCEDURE DIVISION USING str, current-pos, char-to-encode, num-chars.
    MOVE num-chars TO num-chars-disp
    MOVE FUNCTION TRIM(num-chars-disp) TO str (current-pos:3)
    ADD FUNCTION LENGTH(FUNCTION TRIM(num-chars-disp)) TO current-pos
    MOVE char-to-encode TO str (current-pos:1)
    ADD 1 TO current-pos
    .
END PROGRAM add-num-chars.


IDENTIFICATION DIVISION.
FUNCTION-ID. decode.

DATA DIVISION.
LOCAL-STORAGE SECTION.
01  encoded-pos                         PIC 9(3) COMP VALUE 1.
01  decoded-pos                         PIC 9(3) COMP VALUE 1.

01  num-of-char                         PIC 9(3) COMP VALUE 0.

LINKAGE SECTION.
01  encoded                             PIC X(200).

01  decoded                             PIC X(100).

PROCEDURE DIVISION USING encoded RETURNING decoded.
    PERFORM VARYING encoded-pos FROM 1 BY 1
            UNTIL encoded (encoded-pos:2) = SPACES OR encoded-pos > 200
        IF encoded (encoded-pos:1) IS NUMERIC
            COMPUTE num-of-char = num-of-char * 10
                + FUNCTION NUMVAL(encoded (encoded-pos:1))
        ELSE
            PERFORM UNTIL num-of-char = 0
                MOVE encoded (encoded-pos:1) TO decoded (decoded-pos:1)
                ADD 1 TO decoded-pos
                SUBTRACT 1 FROM num-of-char
            END-PERFORM
        END-IF
    END-PERFORM
    .
END FUNCTION decode.
