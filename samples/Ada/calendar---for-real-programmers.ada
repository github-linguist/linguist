WITH PRINTABLE_CALENDAR;

PROCEDURE REAL_CAL IS

   C: PRINTABLE_CALENDAR.CALENDAR := PRINTABLE_CALENDAR.INIT_132
     ((WEEKDAY_REP =>
          "MO TU WE TH FR SA SO",
       MONTH_REP   =>
         ("      JANUARY       ", "      FEBRUARY      ",
          "       MARCH        ", "       APRIL        ",
          "        MAY         ", "        JUNE        ",
          "        JULY        ", "       AUGUST       ",
          "      SEPTEMBER     ", "       OCTOBER      ",
          "      NOVEMBER      ", "      DECEMBER      ")
      ));

BEGIN
   C.PRINT_LINE_CENTERED("[SNOOPY]");
   C.NEW_LINE;
   C.PRINT(1969, "NINETEEN-SIXTY-NINE");
END REAL_CAL;
