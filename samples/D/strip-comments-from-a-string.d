import std.stdio, std.regex;

string remove1LineComment(in string s, in string pat=";#") {
    const re = "([^" ~ pat ~ "]*)([" ~ pat ~ `])[^\n\r]*([\n\r]|$)`;
    return s.replace(regex(re, "gm"), "$1$3");
}

void main() {
    const s = "apples, pears # and bananas
apples, pears ; and bananas ";

    writeln(s, "\n====>\n", s.remove1LineComment());
}
