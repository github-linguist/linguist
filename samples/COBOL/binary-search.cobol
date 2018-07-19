        >>SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. binary-search.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  nums-area                           VALUE "01040612184356".
    03  nums                            PIC 9(2)
                                        OCCURS 7 TIMES
                                        ASCENDING KEY nums
                                        INDEXED BY nums-idx.
PROCEDURE DIVISION.
    SEARCH ALL nums
        WHEN nums (nums-idx) = 4
            DISPLAY "Found 4 at index " nums-idx
    END-SEARCH
    .
END PROGRAM binary-search.
