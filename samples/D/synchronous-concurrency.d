import std.algorithm, std.concurrency, std.stdio;

void main() {
    auto printer = spawn(&printTask, thisTid);
    auto f = File("input.txt","r");
    foreach (string line; lines(f))
        send(printer, line);
    send(printer, true);    //EOF
    auto n = receiveOnly!(int)();
    stdout.writefln("\n%d lines printed.", n);
}

void printTask(Tid reader) {
    int n = 0;
    for (bool eof = false; !eof;)
        receive(
            (string line) {stdout.write(line); n++;},
            (bool) {send(reader, n); eof = true;}
        );
}
