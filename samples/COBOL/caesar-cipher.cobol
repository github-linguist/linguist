       >>SOURCE FORMAT IS FREE
PROGRAM-ID. caesar-cipher.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
    FUNCTION encrypt
    FUNCTION decrypt
    .
DATA DIVISION.
WORKING-STORAGE SECTION.
01  plaintext                 PIC X(50).
01  offset                    PIC 99.

01  encrypted-str             PIC X(50).

PROCEDURE DIVISION.
    DISPLAY "Enter a message to encrypt: " NO ADVANCING
    ACCEPT plaintext
    DISPLAY "Enter the amount to shift by: " NO ADVANCING
    ACCEPT offset

    MOVE FUNCTION encrypt(offset, plaintext) TO encrypted-str
    DISPLAY "Encrypted: " encrypted-str
    DISPLAY "Decrypted: " FUNCTION decrypt(offset, encrypted-str)
    .
END PROGRAM caesar-cipher.


FUNCTION-ID. encrypt.

DATA DIVISION.
LOCAL-STORAGE SECTION.
01  i                         PIC 9(3).

01  a                         PIC 9(3).

LINKAGE SECTION.
01  offset                    PIC 99.
01  str                       PIC X(50).

01  encrypted-str             PIC X(50).

PROCEDURE DIVISION USING offset, str RETURNING encrypted-str.
    MOVE str TO encrypted-str
    PERFORM VARYING i FROM 1 BY 1 UNTIL i > FUNCTION LENGTH(str)
        IF encrypted-str (i:1) IS NOT ALPHABETIC OR encrypted-str (i:1) = SPACE
            EXIT PERFORM CYCLE
        END-IF

        IF encrypted-str (i:1) IS ALPHABETIC-UPPER
            MOVE FUNCTION ORD("A") TO a
        ELSE
            MOVE FUNCTION ORD("a") TO a
        END-IF

        MOVE FUNCTION CHAR(FUNCTION MOD(FUNCTION ORD(encrypted-str (i:1))
                - a + offset, 26) + a)
            TO encrypted-str (i:1)
    END-PERFORM
    .
END FUNCTION encrypt.


FUNCTION-ID. decrypt.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
    FUNCTION encrypt
    .
DATA DIVISION.
LOCAL-STORAGE SECTION.
01  decrypt-offset            PIC 99.

LINKAGE SECTION.
01  offset                    PIC 99.
01  str                       PIC X(50).

01  decrypted-str             PIC X(50).

PROCEDURE DIVISION USING offset, str RETURNING decrypted-str.
    SUBTRACT 26 FROM offset GIVING decrypt-offset
    MOVE FUNCTION encrypt(decrypt-offset, str) TO decrypted-str
    .
END FUNCTION decrypt.
