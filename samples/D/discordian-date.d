import std.stdio, std.datetime, std.conv, std.string;

immutable seasons = ["Chaos", "Discord", "Confusion",
                     "Bureaucracy", "The Aftermath"],
          weekday = ["Sweetmorn", "Boomtime", "Pungenday",
                     "Prickle-Prickle", "Setting Orange"],
          apostle = ["Mungday", "Mojoday", "Syaday",
                     "Zaraday", "Maladay"],
          holiday = ["Chaoflux", "Discoflux", "Confuflux",
                     "Bureflux", "Afflux"];

string discordianDate(in Date date) pure {
    immutable dYear = text(date.year + 1166);

    immutable isLeapYear = date.isLeapYear;
    if (isLeapYear && date.month == 2 && date.day == 29)
        return "St. Tib's Day, in the YOLD " ~ dYear;

    immutable doy = (isLeapYear && date.dayOfYear >= 60) ?
                    date.dayOfYear - 1 :
                    date.dayOfYear;

    immutable dsDay = doy % 73; // Season day.
    if (dsDay == 5)
        return apostle[doy / 73] ~ ", in the YOLD " ~ dYear;
    if (dsDay == 50)
        return holiday[doy / 73] ~ ", in the YOLD " ~ dYear;

    immutable dSeas = seasons[doy / 73];
    immutable dWday = weekday[(doy - 1) % 5];

    return format("%s, day %s of %s in the YOLD %s",
                  dWday, dsDay, dSeas, dYear);
}

unittest {
    assert(Date(2010, 7, 22).discordianDate ==
           "Pungenday, day 57 of Confusion in the YOLD 3176");
    assert(Date(2012, 2, 28).discordianDate ==
           "Prickle-Prickle, day 59 of Chaos in the YOLD 3178");
    assert(Date(2012, 2, 29).discordianDate ==
           "St. Tib's Day, in the YOLD 3178");
    assert(Date(2012, 3, 1).discordianDate ==
           "Setting Orange, day 60 of Chaos in the YOLD 3178");
    assert(Date(2010, 1, 5).discordianDate ==
           "Mungday, in the YOLD 3176");
    assert(Date(2011, 5, 3).discordianDate ==
           "Discoflux, in the YOLD 3177");
}

void main() {
    (cast(Date)Clock.currTime).discordianDate.writeln;
}
