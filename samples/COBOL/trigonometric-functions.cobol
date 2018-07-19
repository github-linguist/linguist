       IDENTIFICATION DIVISION.
       PROGRAM-ID. Trig.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Pi-Third   USAGE COMP-2.
       01  Degree     USAGE COMP-2.

       01  60-Degrees USAGE COMP-2.

       01  Result     USAGE COMP-2.

       PROCEDURE DIVISION.
           COMPUTE Pi-Third = FUNCTION PI / 3

           DISPLAY "Radians:"
           DISPLAY "  Sin(π / 3)  = " FUNCTION SIN(Pi-Third)
           DISPLAY "  Cos(π / 3)  = " FUNCTION COS(Pi-Third)
           DISPLAY "  Tan(π / 3)  = " FUNCTION TAN(Pi-Third)
           DISPLAY "  Asin(0.5)   = " FUNCTION ASIN(0.5)
           DISPLAY "  Acos(0.5)   = " FUNCTION ACOS(0.5)
           DISPLAY "  Atan(0.5)   = " FUNCTION ATAN(0.5)

           COMPUTE Degree = FUNCTION PI / 180
           COMPUTE 60-Degrees = Degree * 60

           DISPLAY "Degrees:"
           DISPLAY "  Sin(60°)  = " FUNCTION SIN(60-Degrees)
           DISPLAY "  Cos(60°)  = " FUNCTION COS(60-Degrees)
           DISPLAY "  Tan(60°)  = " FUNCTION TAN(60-Degrees)
           COMPUTE Result = FUNCTION ASIN(0.5) / 60
           DISPLAY "  Asin(0.5) = " Result
           COMPUTE Result = FUNCTION ACOS(0.5) / 60
           DISPLAY "  Acos(0.5) = " Result
           COMPUTE Result = FUNCTION ATAN(0.5) / 60
           DISPLAY "  Atan(0.5) = " Result

           GOBACK
           .
