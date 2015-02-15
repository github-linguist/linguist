       PROGRAM-ID. memory-allocation.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  based-data              PIC X(20) VALUE "Hello, World!"
                                   BASED.

       PROCEDURE DIVISION.
           *> INITIALIZED sets the data item to the VALUE.
           ALLOCATE based-data INITIALIZED
           DISPLAY based-data
           FREE based-data

           GOBACK
           .
