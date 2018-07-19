filter(Calendar.Year(2008)->range(Calendar.Year(2121))->years()->month(12)->day(25), lambda(object day){ return day->week_day()==7; })->year()->format_nice();
