       IDENTIFICATION DIVISION.
       PROGRAM-ID. arrays.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  fixed-length-table.
           03  fixed-table-elt      PIC X OCCURS 5 TIMES.

       01  table-length             PIC 9(5) VALUE 1.
       01  variable-length-table.
           03  variable-table-elt   PIC X OCCURS 1 TO 5 TIMES
               DEPENDING ON table-length.

       01  initial-value-area.
           03  initial-values.
               05  FILLER           PIC X(10) VALUE "One".
               05  FILLER           PIC X(10) VALUE "Two".
               05  FILLER           PIC X(10) VALUE "Three".
           03 initial-value-table REDEFINES initial-values.
              05  initial-table-elt PIC X(10) OCCURS 3 TIMES.

       01  indexed-table.
           03  indexed-elt          PIC X OCCURS 5 TIMES
               INDEXED BY table-index.

       PROCEDURE DIVISION.
           *> Assigning the contents of an entire table.
           MOVE "12345" TO fixed-length-table

           *>  Indexing an array (using an index)
           MOVE 1 TO table-index
           MOVE "1" TO indexed-elt (table-index)

           *> Pushing a value into a variable-length table.
           ADD 1 TO table-length
           MOVE "1" TO variable-table-elt (2)

           GOBACK
           .
