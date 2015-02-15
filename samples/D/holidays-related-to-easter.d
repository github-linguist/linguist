import std.stdio, std.datetime;

void printEasterRelatedHolidays(in int year) {
    static struct Holyday {
        string name;
        int offs;
    }
    static immutable Holyday[] holidayOffsets = [{"Easter",     0},
                                                 {"Ascension", 39},
                                                 {"Pentecost", 10},
                                                 {"Trinity",    7},
                                                 {"Corpus",     4}];

    // Calculate Easter.
    Date date;
    {
        immutable a = year % 19;
        immutable b = year / 100;
        immutable c = year %100;
        immutable d = b / 4;
        immutable e = b % 4;
        immutable f = (b + 8) / 25;
        immutable g = (b - f + 1) / 3;
        immutable h = (19 * a + b - d - g + 15) % 30;
        immutable i = c / 4;
        immutable k = c % 4;
        immutable l = (32 + 2 * e + 2 * i - h - k) % 7;
        immutable m = (a + 11 * h + 22 * l) / 451;
        immutable n = h + l - 7 * m + 114;
        immutable month = n / 31;
        immutable day = (n % 31) + 1;
        date = Date(year, month, day);
    }

    writef("%4d ", year);
    foreach (immutable hd; holidayOffsets) {
        date += days(hd.offs);
        writef("%s: %2d %s  ", hd.name, date.day, date.month);
    }
    writeln();
}

void main() {
    writeln("Christian holidays, related to Easter," ~
            " for each centennial from 400 to 2100 CE:");
    for (int y = 400; y <= 2100; y += 100)
        printEasterRelatedHolidays(y);

    writeln("\nChristian holidays, related to Easter," ~
            " for years from 2010 to 2020 CE:");
    foreach (immutable y; 2010 .. 2021)
        printEasterRelatedHolidays(y);
}
