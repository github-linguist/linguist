PROGRAM-ID. horizontal-sundial-calc.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  latitude                       PIC S9(3)V9(5) COMP.
01  longitude                      PIC S9(3)V9(5) COMP.
01  legal-meridian                 PIC S9(3)V9(5) COMP.

01  lat-sine                       PIC S9(3)V9(5) COMP.
01  diff-longitude                 PIC S9(3)V9(5) COMP.

01  lat-sine-disp                  PIC -(3)9.9(5).
01  diff-longitude-disp            PIC -(3)9.9(5).

01  hour                           PIC S9 COMP.
01  sun-hour-angle                 PIC S9(3)V9(5) COMP.
01  dial-hour-line-angle           PIC S9(3)V9(5) COMP.

01  hour-disp                      PIC 99.
01  sun-hour-angle-disp            PIC -(3)9.9(5).
01  dial-hour-line-angle-disp      PIC -(3)9.9(5).

PROCEDURE DIVISION.
    DISPLAY "Enter latitude: " NO ADVANCING
    ACCEPT latitude
    DISPLAY "Enter longitude: " NO ADVANCING
    ACCEPT longitude
    DISPLAY "Enter legal meridian: " NO ADVANCING
    ACCEPT legal-meridian
    DISPLAY SPACE

    COMPUTE lat-sine, lat-sine-disp ROUNDED =
        FUNCTION SIN(latitude * 2 * FUNCTION PI / 360)
    DISPLAY "Sine of latitude: " FUNCTION TRIM(lat-sine-disp)

    SUBTRACT legal-meridian FROM longitude
        GIVING diff-longitude, diff-longitude-disp
    DISPLAY "Diff longitude: " FUNCTION TRIM(diff-longitude-disp)
    DISPLAY SPACE

    DISPLAY "Time   Sun hour angle  Dial hour line angle"
    PERFORM VARYING hour FROM -6 BY 1 UNTIL hour > 6
        COMPUTE sun-hour-angle ROUNDED = hour * 15 - diff-longitude
        COMPUTE dial-hour-line-angle ROUNDED = FUNCTION ATAN(lat-sine
            * FUNCTION TAN(sun-hour-angle * 2 * FUNCTION PI / 360))
            * 360 / (2 * FUNCTION PI)

        ADD 12 TO hour GIVING hour-disp
        MOVE sun-hour-angle TO sun-hour-angle-disp
        MOVE dial-hour-line-angle TO dial-hour-line-angle-disp
        DISPLAY hour-disp ":00 " sun-hour-angle-disp "      "
            dial-hour-line-angle-disp
    END-PERFORM
    .
