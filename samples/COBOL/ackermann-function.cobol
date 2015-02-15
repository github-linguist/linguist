       IDENTIFICATION DIVISION.
       PROGRAM-ID. Ackermann.

       DATA DIVISION.
       LINKAGE SECTION.
       01  M          USAGE UNSIGNED-LONG.
       01  N          USAGE UNSIGNED-LONG.

       01  Return-Val USAGE UNSIGNED-LONG.

       PROCEDURE DIVISION USING M N Return-Val.
           EVALUATE M ALSO N
               WHEN 0 ALSO ANY
                   ADD 1 TO N GIVING Return-Val

               WHEN NOT 0 ALSO 0
                   SUBTRACT 1 FROM M
                   CALL "Ackermann" USING BY CONTENT M BY CONTENT 1
                       BY REFERENCE Return-Val

               WHEN NOT 0 ALSO NOT 0
                   SUBTRACT 1 FROM N
                   CALL "Ackermann" USING BY CONTENT M BY CONTENT N
                       BY REFERENCE Return-Val

                   SUBTRACT 1 FROM M
                   CALL "Ackermann" USING BY CONTENT M
                       BY CONTENT Return-Val BY REFERENCE Return-Val
           END-EVALUATE

           GOBACK
           .
