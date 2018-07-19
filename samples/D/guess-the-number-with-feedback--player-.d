import std.stdio, std.string;

void main() {
    immutable mnOrig = 1, mxOrig = 10;
    int mn = mnOrig, mx = mxOrig;

    writefln(
    "Think of a number between %d and %d and wait for me to guess it.
    On every guess of mine you should state whether the guess was
    too high, too low, or equal to your number by typing h, l, or =",
            mn, mx);

    LOOP: for (int i = 1; ; i++) {
        immutable guess = (mn + mx) / 2;
        writef("Guess %2d is: %2d. The score for which is (h,l,=): ",
               i, guess);
        immutable string txt = readln().strip().toLower();

        switch (txt) {
            case "h":
                mx = guess - 1;
                break;
            case "l":
                mn = guess + 1;
                break;
            case "=":
                writeln("  Yeehaw!!");
                break LOOP;
            default:
                writefln("  I don't understand your input '%s'.",
                         txt);
                continue LOOP;
        }

        if (mn > mx || mn < mnOrig || mx > mxOrig) {
            writeln("Please check your scoring as" ~
                    " I cannot find the value");
            break;
        }
    }
    writeln("\nThanks for keeping score.");
}
