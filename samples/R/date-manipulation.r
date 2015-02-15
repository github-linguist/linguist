time <- strptime("March 7 2009 7:30pm EST", "%B %d %Y %I:%M%p %Z") # "2009-03-07 19:30:00"
isotime <- ISOdatetime(1900 + time$year, time$mon, time$mday,
   time$hour, time$min, time$sec, "EST")                           # "2009-02-07 19:30:00 EST"
twelvehourslater <- isotime + 12 * 60 * 60                         # "2009-02-08 07:30:00 EST"
timeincentraleurope <- format(isotime, tz="CET", usetz=TRUE)       #"2009-02-08 01:30:00 CET"
