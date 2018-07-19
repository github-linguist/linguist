       IDENTIFICATION DIVISION.
       PROGRAM-ID. temp-conversion.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  Kelvin-Rankine-Ratio    VALUE 0.5556. *> 5 / 9 to 4 d.p.
       78  Kelvin-Celsius-Diff     VALUE 273.15.
       78  Rankine-Fahrenheit-Diff VALUE 459.67.

       01  temp-kelvin             PIC S9(8)V99.
       01  temp-rankine            PIC S9(8)V99.

       01  kelvin                  PIC -(7)9.99.
       01  celsius                 PIC -(7)9.99.
       01  rankine                 PIC -(7)9.99.
       01  fahrenheit              PIC -(7)9.99.

       PROCEDURE DIVISION.
           DISPLAY "Enter a temperature in Kelvin to convert: " NO ADVANCING
           ACCEPT temp-kelvin

           MOVE temp-kelvin TO kelvin
           DISPLAY "K " kelvin

           SUBTRACT Kelvin-Celsius-Diff FROM temp-kelvin GIVING celsius
           DISPLAY "C " celsius

           DIVIDE temp-kelvin BY Kelvin-Rankine-Ratio
               GIVING temp-rankine, rankine
           SUBTRACT Rankine-Fahrenheit-Diff FROM temp-rankine GIVING fahrenheit

           DISPLAY "F " fahrenheit
           DISPLAY "R " rankine

           GOBACK
           .
