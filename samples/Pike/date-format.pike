> write("%d-%02d-%02d\n", day->year_no(), day->month_no(), day->month_day());
2011-11-05

> write("%s, %s %s, %s\n", day->week_day_name(), day->month_name(), day->month_day_name(), day->year_name());
Saturday, November 5, 2011

> write(day->format_ymd()+"\n");
2011-11-05

> write(day->format_ext_ymd()+"\n");
Saturday, 5 November 2011
