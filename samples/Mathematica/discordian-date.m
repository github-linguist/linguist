DiscordianDate[y_, m_, d_] := Module[{year = ToString[y + 1166], month = m, day = d},

  DMONTHS = {"Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath"};
  DDAYS = {"Sweetmorn", "Boomtime", "Pungenday", "Prickle-Prickle", "Setting Orange"};
  DayOfYear = DateDifference[{y} ,{y, m, d}] + 1;
  LeapYearQ = (Mod[#, 4]== 0 && (Mod[#, 100] != 0 || Mod[#, 400] == 0))&@ y;

  If [ LeapYearQ && month == 2 && day == 29,
    Print["Today is St. Tib's Day, YOLD ", ]
  ,
    If [ LeapYearQ && DayOfYear >= 60, DayOfYear -= 1 ];
     {season, dday} = {Quotient[DayOfYear, 73], Mod[DayOfYear, 73]};
     Print["Today is ", DDAYS[[Mod[dday,4] + 1]],", ",DMONTHS[[season+1]]," ",dday,", YOLD ",year]
    ];
]
