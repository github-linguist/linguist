       IDENTIFICATION DIVISION.
       PROGRAM-ID. Strip-Chars.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Str-Size  CONSTANT 128.

       LOCAL-STORAGE SECTION.
       01  I       PIC 999.
       01  Str-Pos PIC 999.

       01  Offset  PIC 999.
       01  New-Pos PIC 999.

       01  Str-End PIC 999.

       LINKAGE SECTION.
       01  Str     PIC X(Str-Size).
       01  Chars-To-Replace PIC X(256).

       PROCEDURE DIVISION USING Str BY VALUE Chars-To-Replace.
       Main.
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL Chars-To-Replace (I:1) = X"00"

               MOVE ZERO TO Offset

*              *> Overwrite the characters to remove by left-shifting
*              *> following characters over them.
               PERFORM VARYING Str-Pos FROM 1 BY 1
                       UNTIL Str-Size < Str-Pos
                   IF Str (Str-Pos:1) = Chars-To-Replace (I:1)
                       ADD 1 TO Offset
                   ELSE IF Offset NOT = ZERO
                       COMPUTE New-Pos = Str-Pos - Offset
                       MOVE Str (Str-Pos:1) TO Str (New-Pos:1)
                   END-IF
               END-PERFORM

*              *> Move spaces to characters at the end that have been
*              *> shifted over.
               COMPUTE Str-End = Str-Size - Offset
               MOVE SPACES TO Str (Str-End:Offset)
           END-PERFORM

           GOBACK
           .
