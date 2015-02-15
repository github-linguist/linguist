void main() {
    import std.stdio, std.random, std.string, std.algorithm,
           std.range, std.conv, std.ascii;

    immutable hidden = "123456789"d.randomCover.take(4).array;
    while (true) {
        "Next guess: ".write;
        immutable d = readln.strip.dtext;
        if (d.count == 4 && d.all!isDigit &&
            d.dup.sort().uniq.count == 4) {
            immutable bulls = d.zip(hidden).count!(p => p[0] == p[1]);
            if (bulls == 4)
                return " You guessed it!".writeln;
            immutable cows = d.count!(g => hidden.canFind(g)) - bulls;
            writefln("bulls %d, cows %d", bulls, cows);
        }
        " Bad guess! (4 unique digits, 1-9)".writeln;
    }
}
