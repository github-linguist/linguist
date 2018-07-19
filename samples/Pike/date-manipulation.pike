> (Calendar.dwim_time("March 7 2009 7:30pm EST")+Calendar.Hour()*12)->set_timezone("CET")->format_ext_time();
Result: "Saturday, 7 March 2009 12:30:00"
