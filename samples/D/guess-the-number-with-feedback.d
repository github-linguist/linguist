import std.stdio, std.random, std.typecons, std.conv, std.string,
       std.range;

void main() {
    immutable interval = tuple(1, 100);
    writefln("Guess my target number that is between " ~
             "%d and %d (inclusive).\n", interval[]);
    immutable target = uniform!"[]"(interval[]);

    foreach (immutable i; sequence!q{n}) {
        writef("Your guess #%d: ", i + 1);
        immutable txt = stdin.readln.strip;

        Nullable!int answer;
        try {
            answer = txt.to!int;
        } catch (ConvException e) {
            writefln("  I don't understand your input '%s'", txt);
            continue;
        }
        if (answer < interval[0] || answer > interval[1]) {
            writeln("  Out of range!");
            continue;
        }
        if (answer == target) {
            writeln("  Well guessed.");
            break;
        }
        writeln(answer < target ? "  Too low." : "  Too high.");
    }
}
