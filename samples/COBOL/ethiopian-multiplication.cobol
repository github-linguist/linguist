       *>* Ethiopian multiplication

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ethiopian-multiplication.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  l                  PICTURE 9(10) VALUE 17.
       01  r                  PICTURE 9(10) VALUE 34.
       01  ethiopian-multiply PICTURE 9(20).
       01  product            PICTURE 9(20).
       PROCEDURE DIVISION.
         CALL "ethiopian-multiply" USING
           BY CONTENT l, BY CONTENT r,
           BY REFERENCE ethiopian-multiply
         END-CALL
         DISPLAY ethiopian-multiply END-DISPLAY
         MULTIPLY l BY r GIVING product END-MULTIPLY
         DISPLAY product END-DISPLAY
         STOP RUN.
       END PROGRAM ethiopian-multiplication.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ethiopian-multiply.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  evenp   PICTURE 9.
         88 even   VALUE 1.
         88 odd    VALUE 0.
       LINKAGE SECTION.
       01  l       PICTURE 9(10).
       01  r       PICTURE 9(10).
       01  product PICTURE 9(20) VALUE ZERO.
       PROCEDURE DIVISION using l, r, product.
         MOVE ZEROES TO product
         PERFORM UNTIL l EQUAL ZERO
           CALL "evenp" USING
             BY CONTENT l,
             BY REFERENCE evenp
           END-CALL
           IF odd
             ADD r TO product GIVING product END-ADD
           END-IF
           CALL "halve" USING
             BY CONTENT l,
             BY REFERENCE l
           END-CALL
           CALL "twice" USING
             BY CONTENT r,
             BY REFERENCE r
           END-CALL
         END-PERFORM
         GOBACK.
       END PROGRAM ethiopian-multiply.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. halve.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01  n   PICTURE 9(10).
       01  m   PICTURE 9(10).
       PROCEDURE DIVISION USING n, m.
         DIVIDE n BY 2 GIVING m END-DIVIDE
         GOBACK.
       END PROGRAM halve.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. twice.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01  n   PICTURE 9(10).
       01  m   PICTURE 9(10).
       PROCEDURE DIVISION USING n, m.
         MULTIPLY n by 2 GIVING m END-MULTIPLY
         GOBACK.
       END PROGRAM twice.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. evenp.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  q   PICTURE 9(10).
       LINKAGE SECTION.
       01  n   PICTURE 9(10).
       01  m   PICTURE 9(1).
         88 even   VALUE 1.
         88 odd    VALUE 0.
       PROCEDURE DIVISION USING n, m.
         DIVIDE n BY 2 GIVING q REMAINDER m END-DIVIDE
         SUBTRACT m FROM 1 GIVING m END-SUBTRACT
         GOBACK.
       END PROGRAM evenp.
