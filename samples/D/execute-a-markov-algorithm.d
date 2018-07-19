void main() {
    import std.stdio, std.file, std.regex, std.string, std.range,
           std.functional;

    const rules = "markov_rules.txt".readText.splitLines.split("");
    auto tests = "markov_tests.txt".readText.splitLines;
    auto re = ctRegex!(r"^([^#]*?)\s+->\s+(\.?)(.*)"); // 160 MB RAM.

    alias slZip = curry!(zip, StoppingPolicy.requireSameLength);
    foreach (test, const rule; slZip(tests, rules)) {
        const origTest = test.dup;

        string[][] capt;
        foreach (const line; rule) {
            auto m = line.match(re);
            if (!m.empty) {
                //capt.put(m.captures.dropOne);
                capt ~= m.captures.dropOne.array;
            }
        }

    REDO:
        const copy = test;
        foreach (const c; capt) {
            test = test.replace(c[0], c[2]);
            if (c[1] == ".")
                break;
            if (test != copy)
                goto REDO;
        }
        writefln("%s\n%s\n", origTest, test);
    }
}
