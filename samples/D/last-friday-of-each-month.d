import std.stdio, std.datetime, std.traits;

void lastFridays(in uint year) {
    auto date = Date(year, 1, 1);
    foreach (_; [EnumMembers!Month]) {
        date.day(date.daysInMonth);
        date.roll!"days"(-(date.dayOfWeek + 2) % 7);
        writeln(date);
        date.add!"months"(1, AllowDayOverflow.no);
    }
}

void main() {
    lastFridays(2012);
}
