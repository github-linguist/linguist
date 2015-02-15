       IDENTIFICATION DIVISION.
       PROGRAM-ID. Map.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Table-Size CONSTANT 30.

       LOCAL-STORAGE SECTION.
       01  I USAGE UNSIGNED-INT.

       LINKAGE SECTION.
       01  Table-Param.
           03  Table-Values USAGE COMP-2 OCCURS Table-Size TIMES.

       01  Func-Id PIC X(30).

       PROCEDURE DIVISION USING Table-Param Func-Id.
           PERFORM VARYING I FROM 1 BY 1 UNTIL Table-Size < I
               CALL Func-Id USING BY REFERENCE Table-Values (I)
           END-PERFORM

           GOBACK
           .
