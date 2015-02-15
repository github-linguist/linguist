void main() {
    import std.stdio, std.algorithm, std.conv, std.array, std.regex,
           std.typecons, std.net.curl;

    immutable r1 = `"title":"Category:([^"]+)"`;
    const languages = get("www.rosettacode.org/w/api.php?action=query"~
                          "&list=categorymembers&cmtitle=Category:Pro"~
                          "gramming_Languages&cmlimit=500&format=json")
                      .matchAll(r1).map!q{ a[1].dup }.array;

    auto pairs = get("www.rosettacode.org/w/index.php?" ~
                      "title=Special:Categories&limit=5000")
                  .matchAll(`title="Category:([^"]+)">[^<]+` ~
                            `</a>[^(]+\((\d+) members\)`)
                  .filter!(m => languages.canFind(m[1]))
                  .map!(m => tuple(m[2].to!uint, m[1].dup));

    foreach (i, res; pairs.array.sort!q{a > b}.release)
        writefln("%3d. %3d - %s", i + 1, res[]);
}
