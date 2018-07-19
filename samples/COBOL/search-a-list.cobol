*> This is written to COBOL85, which does not include exceptions.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Search-List.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  haystack-area.
           78  Haystack-Size VALUE 10.
           03  haystack-data.
               05  FILLER     PIC X(7) VALUE "Zig".
               05  FILLER     PIC X(7) VALUE "Zag".
               05  FILLER     PIC X(7) VALUE "Wally".
               05  FILLER     PIC X(7) VALUE "Ronald".
               05  FILLER     PIC X(7) VALUE "Bush".
               05  FILLER     PIC X(7) VALUE "Krusty".
               05  FILLER     PIC X(7) VALUE "Charlie".
               05  FILLER     PIC X(7) VALUE "Bush".
               05  FILLER     PIC X(7) VALUE "Boz".
               05  FILLER     PIC X(7) VALUE "Zag".

           03  haystack-table REDEFINES haystack-data.
               05  haystack   PIC X(7) OCCURS Haystack-Size TIMES
                   INDEXED BY haystack-index.

       01  needle             PIC X(7).

       PROCEDURE DIVISION.
       main.
           MOVE "Bush" TO needle
           PERFORM find-needle

           MOVE "Goofy" TO needle
           PERFORM find-needle

*          *> Extra task
           MOVE "Bush" TO needle
           PERFORM find-last-of-needle

           GOBACK
           .

       find-needle.
           SEARCH haystack
               AT END
                   DISPLAY needle " not found."

               WHEN haystack (haystack-index) = needle
                   DISPLAY "Found " needle " at " haystack-index "."
           END-SEARCH
           .

       find-last-of-needle.
           PERFORM VARYING haystack-index FROM Haystack-Size BY -1
               UNTIL haystack-index = 0
               OR haystack (haystack-index) = needle
           END-PERFORM

           IF haystack-index = 0
               DISPLAY needle " not found."
           ELSE
               DISPLAY "Found last of " needle " at " haystack-index "."
           END-IF
           .
