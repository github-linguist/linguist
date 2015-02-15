*> Apologies for the repetitiveness.
       IDENTIFICATION  DIVISION.
       PROGRAM-ID. coloured-text.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  example-str VALUE "COBOL".

       01  fore-colour PIC 9.
       01  back-colour PIC 9.

       01  line-num    PIC 99 VALUE 1.
       01  col-num     PIC 99 VALUE 1.

       01  pause       PIC X.

       PROCEDURE DIVISION.
           PERFORM VARYING fore-colour FROM 0 BY 1 UNTIL fore-colour > 7
               PERFORM VARYING back-colour FROM 0 BY 1
                       UNTIL back-colour > 7
                   DISPLAY example-str AT LINE line-num, COLUMN col-num
                       WITH FOREGROUND-COLOR fore-colour,
                       BACKGROUND-COLOR back-colour

                   ADD 6 TO col-num
               END-PERFORM

               ADD 1 TO line-num
               MOVE 1 TO col-num
           END-PERFORM

           DISPLAY "With HIGHLIGHT:" AT LINE line-num, COLUMN 1
           ADD 1 TO line-num

           PERFORM VARYING fore-colour FROM 0 BY 1 UNTIL fore-colour > 7
               PERFORM VARYING back-colour FROM 0 BY 1
                       UNTIL back-colour > 7
                   DISPLAY example-str AT LINE line-num, COLUMN col-num
                       WITH FOREGROUND-COLOR fore-colour,
                       BACKGROUND-COLOR back-colour HIGHLIGHT

                   ADD 6 TO col-num
               END-PERFORM

               ADD 1 TO line-num
               MOVE 1 TO col-num
           END-PERFORM

           DISPLAY "With LOWLIGHT: (has no effect on many terminals)"
               AT LINE line-num, COLUMN 1
           ADD 1 TO line-num

           PERFORM VARYING fore-colour FROM 0 BY 1 UNTIL fore-colour > 7
               PERFORM VARYING back-colour FROM 0 BY 1
                       UNTIL back-colour > 7
                   DISPLAY example-str AT LINE line-num, COLUMN col-num
                       WITH FOREGROUND-COLOR fore-colour,
                       BACKGROUND-COLOR back-colour LOWLIGHT

                   ADD 6 TO col-num
               END-PERFORM

               ADD 1 TO line-num
               MOVE 1 TO col-num
           END-PERFORM

           DISPLAY "With BLINK:" AT LINE line-num, COLUMN 1
           ADD 1 TO line-num

           PERFORM VARYING fore-colour FROM 0 BY 1 UNTIL fore-colour > 7
               PERFORM VARYING back-colour FROM 0 BY 1
                       UNTIL back-colour > 7
                   DISPLAY example-str AT LINE line-num, COLUMN col-num
                       WITH FOREGROUND-COLOR fore-colour,
                       BACKGROUND-COLOR back-colour BLINK

                   ADD 6 TO col-num
               END-PERFORM

               ADD 1 TO line-num
               MOVE 1 TO col-num
           END-PERFORM

           DISPLAY "Press enter to continue."
               AT LINE line-num, COLUMN 1
           ACCEPT pause AT LINE line-num, COLUMN 40

           GOBACK
           .
