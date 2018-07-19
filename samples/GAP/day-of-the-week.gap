Filtered([2008 .. 2121], y -> WeekDay([25, 12, y]) = "Sun");
# [ 2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118 ]

# A possible implementation of WeekDayAlt

days := ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"];;

WeekDayAlt := function(args)
   local d, m, y, k;
   d := args[1];
   m := args[2];
   y := args[3];
   if m < 3 then
      m := m + 12;
      y := y - 1;
   fi;
   k := 1 + RemInt(d + QuoInt((m + 1)*26, 10) + y + QuoInt(y, 4)
          + 6*QuoInt(y, 100) + QuoInt(y, 400) + 5, 7);
   return days[k];
end;

Filtered([2008 .. 2121], y -> WeekDayAlt([25, 12, y]) = "Sun");
# [ 2011, 2016, 2022, 2033, 2039, 2044, 2050, 2061, 2067, 2072, 2078, 2089, 2095, 2101, 2107, 2112, 2118 ]
