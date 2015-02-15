import std.stdio, std.regex, std.algorithm, std.string, std.array;

void main() {
    writeln("Enter a story template, terminated by an empty line:");
    string story;
    while (true) {
        auto line = stdin.readln().strip();
        if (line.empty) break;
        story ~= line ~ "\n";
    }

    auto re = regex("<.+?>", "g");
    auto fields = story.match(re).map!q{a.hit}().array().sort().uniq();
    foreach (field; fields) {
        writef("Enter a value for '%s': ", field[1 .. $ - 1]);
        story = story.replace(field, stdin.readln().strip());
    }

    writeln("\nThe story becomes:\n", story);
}
