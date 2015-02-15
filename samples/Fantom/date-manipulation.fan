fansh> d := DateTime.fromLocale("March 7 2009 7:30pm EST", "MMMM D YYYY h:mmaa zzz")
fansh> d
2009-03-07T19:30:00-05:00 EST
fansh> d + 12hr
2009-03-08T07:30:00-05:00 EST
fansh> (d+12hr).toTimeZone(TimeZone("London")) // the extra credit!
2009-03-08T12:30:00Z London
