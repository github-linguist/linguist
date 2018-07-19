import std.stdio, std.algorithm, std.conv, std.range;

struct Employee {
  string name, id;
  uint salary;
  string department;
}

immutable Employee[] data = [
    {"Tyler Bennett",   "E10297", 32_000, "D101"},
    {"John Rappl",      "E21437", 47_000, "D050"},
    {"George Woltman",  "E00127", 53_500, "D101"},
    {"Adam Smith",      "E63535", 18_000, "D202"},
    {"Claire Buckman",  "E39876", 27_800, "D202"},
    {"David McClellan", "E04242", 41_500, "D101"},
    {"Rich Holcomb",    "E01234", 49_500, "D202"},
    {"Nathan Adams",    "E41298", 21_900, "D050"},
    {"Richard Potter",  "E43128", 15_900, "D101"},
    {"David Motsinger", "E27002", 19_250, "D202"},
    {"Tim Sampair",     "E03033", 27_000, "D101"},
    {"Kim Arlich",      "E10001", 57_000, "D190"},
    {"Timothy Grove",   "E16398", 29_900, "D190"}];

void main(in string[] args) {
  immutable n = (args.length == 2) ? to!int(args[1]) : 3;

  Employee[][string] departments;
  foreach (immutable rec; data)
    departments[rec.department] ~= rec;

  foreach (dep, recs; departments) {
    recs.topN!q{a.salary > b.salary}(n);
    writefln("Department %s\n  %(%s\n  %)\n", dep, recs.take(n));
  }
}
