       IDENTIFICATION DIVISION.
       PROGRAM-ID. quicksort RECURSIVE.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  temp                   PIC S9(8).

       01  pivot                  PIC S9(8).

       01  left-most-idx          PIC 9(5).
       01  right-most-idx         PIC 9(5).

       01  left-idx               PIC 9(5).
       01  right-idx              PIC 9(5).

       LINKAGE SECTION.
       78  Arr-Length             VALUE 50.

       01  arr-area.
           03  arr                PIC S9(8) OCCURS Arr-Length TIMES.

       01  left-val               PIC 9(5).
       01  right-val              PIC 9(5).

       PROCEDURE DIVISION USING REFERENCE arr-area, OPTIONAL left-val,
               OPTIONAL right-val.
           IF left-val IS OMITTED OR right-val IS OMITTED
               MOVE 1 TO left-most-idx, left-idx
               MOVE Arr-Length TO right-most-idx, right-idx
           ELSE
               MOVE left-val TO left-most-idx, left-idx
               MOVE right-val TO right-most-idx, right-idx
           END-IF

           IF right-most-idx - left-most-idx < 1
               GOBACK
           END-IF

           COMPUTE pivot = arr ((left-most-idx + right-most-idx) / 2)

           PERFORM UNTIL left-idx > right-idx
               PERFORM VARYING left-idx FROM left-idx BY 1
                   UNTIL arr (left-idx) >= pivot
               END-PERFORM

               PERFORM VARYING right-idx FROM right-idx BY -1
                   UNTIL arr (right-idx) <= pivot
               END-PERFORM

               IF left-idx <= right-idx
                   MOVE arr (left-idx) TO temp
                   MOVE arr (right-idx) TO arr (left-idx)
                   MOVE temp TO arr (right-idx)

                   ADD 1 TO left-idx
                   SUBTRACT 1 FROM right-idx
               END-IF
           END-PERFORM

           CALL "quicksort" USING REFERENCE arr-area,
               CONTENT left-most-idx, right-idx
           CALL "quicksort" USING REFERENCE arr-area, CONTENT left-idx,
               right-most-idx

           GOBACK
           .
