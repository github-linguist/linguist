import
    std.conv,
    std.stdio;

enum Color {
    fgBlack = 30,
    fgRed,
    fgGreen,
    fgYellow,
    fgBlue,
    fgMagenta,
    fgCyan,
    fgWhite,

    bgBlack = 40,
    bgRed,
    bgGreen,
    bgYellow,
    bgBlue,
    bgMagenta,
    bgCyan,
    bgWhite
}

string color(string text, Color ink) {
    return "\033["
        ~ ink.to!int.to!string
        ~ "m"
        ~ text
        ~ "\033[0m";
}

void main() {
    auto colors = [
        Color.fgBlack,
        Color.fgRed,
        Color.fgGreen,
        Color.fgYellow,
        Color.fgBlue,
        Color.fgMagenta,
        Color.fgCyan,
        Color.fgWhite
    ];

    foreach (c; colors) {
        // Print the color name, in white.
        c.to!string.color(Color.fgWhite).writeln;

        // Print some text in the color.
        "Hello, world!".color(c).writeln;
    }
}
