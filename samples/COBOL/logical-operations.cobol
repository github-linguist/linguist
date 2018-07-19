       IDENTIFICATION DIVISION.
       PROGRAM-ID. print-logic.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  result                  PIC 1 USAGE BIT.

       LINKAGE SECTION.
       01  a                       PIC 1 USAGE BIT.
       01  b                       PIC 1 USAGE BIT.

       PROCEDURE DIVISION USING a, b.
           COMPUTE result = a B-AND b
           DISPLAY "a and b is " result

           COMPUTE result = a B-OR b
           DISPLAY "a or b is " result

           COMPUTE result = B-NOT a
           DISPLAY "Not a is " result

           COMPUTE result = a B-XOR b
           DISPLAY "a exclusive-or b is " result

           GOBACK
           .
