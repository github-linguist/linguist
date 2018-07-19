       IDENTIFICATION DIVISION.
       PROGRAM-ID. Test-Prompt-Menu.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  Num-Options    USAGE UNSIGNED-INT VALUE 4.
       01  Example-Menu.
           03  Example-Options-Data.
               05  FILLER PIC X(30) VALUE "fee fie".
               05  FILLER PIC X(30) VALUE "huff and puff".
               05  FILLER PIC X(30) VALUE "mirror mirror".
               05  FILLER PIC X(30) VALUE "tick tock".

           03  Example-Options-Values REDEFINES Example-Options-Data.
               05  Example-Options PIC X(30) OCCURS 4 TIMES.

       01  Chosen-Option PIC X(30).

       PROCEDURE DIVISION.
           CALL "Prompt-Menu" USING BY CONTENT Num-Options
               BY CONTENT Example-Menu
               BY REFERENCE Chosen-Option

           DISPLAY "You chose: " Chosen-Option

           GOBACK
           .

       END PROGRAM Test-Prompt-Menu.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. Prompt-Menu.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  User-Input        USAGE UNSIGNED-INT.
       01  Input-Flag        PIC X.
           88  Valid-Input   VALUE "Y".

       01  Options-Index     USAGE UNSIGNED-INT.
       01  Index-Display     PIC Z(10).

       LINKAGE SECTION.

       01  Num-Options       USAGE UNSIGNED-INT.
       01  Menu-Options.
           03  Options-Table PIC X(30) OCCURS 0 TO 10000000 TIMES
               DEPENDING ON Num-Options.

       01  Chosen-Option     PIC X(30).

       PROCEDURE DIVISION USING Num-Options Menu-Options Chosen-Option.
       Main.
           IF Num-Options = 0
               MOVE SPACES TO Chosen-Option
               GOBACK
           END-IF

           PERFORM UNTIL Valid-Input
               PERFORM Display-Menu-Options

               DISPLAY "Choose an option: " WITH NO ADVANCING
               ACCEPT User-Input

               PERFORM Validate-Input
           END-PERFORM

           MOVE Options-Table (User-Input) TO Chosen-Option

           GOBACK
           .

       Display-Menu-Options.
           PERFORM VARYING Options-Index FROM 1 BY 1
                   UNTIL Num-Options < Options-Index
               MOVE Options-Index TO Index-Display
               DISPLAY
                   Index-Display ". " Options-Table (Options-Index)
               END-DISPLAY
           END-PERFORM
           .

       Validate-Input.
           IF User-Input = 0 OR > Num-Options
               DISPLAY "Invalid input."
           ELSE
               SET Valid-Input TO TRUE
           END-IF
           .

       END PROGRAM Prompt-Menu.
