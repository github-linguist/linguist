       IDENTIFICATION DIVISION.
       PROGRAM-ID. test-balanced-brackets.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  True-Val  CONSTANT 0.
       01  False-Val CONSTANT 1.

       LOCAL-STORAGE SECTION.
       01  current-time        PIC 9(10).

       01  bracket-type        PIC 9.
           88 add-open-bracket VALUE 1.

       01  bracket-string-area.
           03  bracket-string  PIC X(10) OCCURS 10 TIMES.

       01  i                   PIC 999.
       01  j                   PIC 999.

       PROCEDURE DIVISION.
           *> Seed RANDOM().
           MOVE FUNCTION CURRENT-DATE (7:10) TO current-time
           MOVE FUNCTION RANDOM(current-time) TO current-time


           *> Generate random strings of brackets.
           PERFORM VARYING i FROM 1 BY 1 UNTIL 10 < i
               PERFORM VARYING j FROM 1 BY 1 UNTIL i < j
                   COMPUTE bracket-type =
                       FUNCTION REM(FUNCTION RANDOM * 1000, 2)

                   IF add-open-bracket
                       MOVE "[" TO bracket-string (i) (j:1)
                   ELSE
                       MOVE "]" TO bracket-string (i) (j:1)
                   END-IF
               END-PERFORM
           END-PERFORM

           *> Display if the strings are balanced or not.
           PERFORM VARYING i FROM 1 BY 1 UNTIL 10 < i
               CALL "check-if-balanced" USING bracket-string (i)
               IF RETURN-CODE = True-Val
                   DISPLAY FUNCTION TRIM(bracket-string (i))
                       " is balanced."
               ELSE
                   DISPLAY FUNCTION TRIM(bracket-string (i))
                       " is not balanced."
               END-IF
           END-PERFORM

           GOBACK
           .

       END PROGRAM test-balanced-brackets.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. check-if-balanced.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  True-Val  CONSTANT 0.
       01  False-Val CONSTANT 1.

       LOCAL-STORAGE SECTION.
       01  nesting-level  PIC S999.
       01  i              PIC 999.

       LINKAGE SECTION.
       01  bracket-string PIC X(100).

       PROCEDURE DIVISION USING bracket-string.
           PERFORM VARYING i FROM 1 BY 1
                   UNTIL (100 < i)
                      OR (bracket-string (i:1) = SPACE)
                      OR (nesting-level < 0)
               IF bracket-string (i:1) = "["
                   ADD 1 TO nesting-level
               ELSE
                   SUBTRACT 1 FROM nesting-level
                   IF nesting-level < 0
                       MOVE False-Val TO RETURN-CODE
                       GOBACK
                   END-IF
               END-IF
           END-PERFORM

           IF nesting-level = 0
               MOVE True-Val TO RETURN-CODE
           ELSE
               MOVE False-Val TO RETURN-CODE
           END-IF

           GOBACK
           .

       END PROGRAM check-if-balanced.
