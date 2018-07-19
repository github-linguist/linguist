       IDENTIFICATION DIVISION.
       PROGRAM-ID. testing.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  occurrences             PIC 99.

       PROCEDURE DIVISION.
           INSPECT "the three truths" TALLYING occurrences FOR ALL "th"
           DISPLAY occurrences

           MOVE 0 TO occurrences
           INSPECT "ababababab" TALLYING occurrences FOR ALL "abab"
           DISPLAY occurrences

           MOVE 0 TO occurrences
           INSPECT "abaabba*bbaba*bbab" TALLYING occurrences
               FOR ALL "a*b"
           DISPLAY occurrences

           GOBACK
           .
