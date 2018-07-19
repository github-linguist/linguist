import std.stdio, std.string, std.algorithm, std.path, std.array;

string commonDirPath(in string[] paths, in string sep = "/") pure {
    if (paths.empty)
        return null;
    return paths.map!(p => p.split(sep)).reduce!commonPrefix.join(sep);
}

void main() {
    immutable paths = ["/home/user1/tmp/coverage/test",
                       "/home/user1/tmp/covert/operator",
                       "/home/user1/tmp/coven/members"];
    writeln(`The common path is: "`, paths.commonDirPath, '"');
}
