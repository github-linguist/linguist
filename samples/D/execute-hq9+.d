import std.stdio, std.string;

void main(in string[] args) {
    if (args.length != 2 ||
        args[1].length != args[1].countchars("hHqQ9+")) {
        writeln("Not valid HQ9+ code.");
        return;
    }

    ulong accumulator;
    foreach (immutable c; args[1]) {
        final switch(c) {
            case 'Q', 'q':
                writeln(args[1]);
                break;
            case 'H', 'h':
                writeln("Hello, world!");
                break;
            case '9':
                int bottles = 99;
                while (bottles > 1) {
                    writeln(bottles, " bottles of beer on the wall,");
                    writeln(bottles, " bottles of beer.");
                    writeln("Take one down, pass it around,");
                    if (--bottles > 1)
                        writeln(bottles,
                                " bottles of beer on the wall.\n");
                }
                writeln("1 bottle of beer on the wall.\n");
                break;
            case '+':
                accumulator++;
                break;
        }
    }
}
