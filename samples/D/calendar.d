import std.stdio, std.datetime, std.string, std.conv;

void printCalendar(in uint year, in uint nCols)
in {
    assert(nCols > 0 && nCols <= 12);
} body {
    immutable rows = 12 / nCols + (12 % nCols != 0);
    auto date = Date(year, 1, 1);
    auto offs = cast(int)date.dayOfWeek;
    const months = "January February March April May June
        July August September October November December".split;

    string[8][12] mons;
    foreach (immutable m; 0 .. 12) {
        mons[m][0] = months[m].center(21);
        mons[m][1] = " Su Mo Tu We Th Fr Sa";
        immutable dim = date.daysInMonth;
        foreach (immutable d; 1 .. 43) {
            immutable day = d > offs && d <= offs + dim;
            immutable str = day ? format(" %2s", d-offs) : "   ";
            mons[m][2 + (d - 1) / 7] ~= str;
        }
        offs = (offs + dim) % 7;
        date.add!"months"(1);
    }

    "[Snoopy Picture]".center(nCols * 24 + 4).writeln;
    writeln(year.text.center(nCols * 24 + 4), "\n");
    foreach (immutable r; 0 .. rows) {
        string[8] s;
        foreach (immutable c; 0 .. nCols) {
            if (r * nCols + c > 11)
                break;
            foreach (immutable i, line; mons[r * nCols + c])
                s[i] ~= format("   %s", line);
        }
        writefln("%-(%s\n%)\n", s);
    }
}

void main() {
    printCalendar(1969, 3);
}
