import std.stdio, std.algorithm;

struct Pair { string name, value; }

void main() {
    Pair[] pairs = [{"Joe",    "5531"},
                    {"Adam",   "2341"},
                    {"Bernie",  "122"},
                    {"Walter", "1234"},
                    {"David",    "19"}];

    pairs.schwartzSort!q{ a.name }.writeln;
}
