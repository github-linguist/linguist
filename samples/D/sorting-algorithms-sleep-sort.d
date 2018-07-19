import std.stdio, std.conv, std.datetime, core.thread;

final class SleepSorter: Thread {
    private immutable uint val;

    this(in uint n) /*pure nothrow*/ {
        super(&run);
        val = n;
    }

    private void run() {
        Thread.sleep(dur!"msecs"(1000 * val));
        writef("%d ", val);
    }
}

void main(in string[] args) {
    if (args.length > 1)
        foreach (arg; args[1 .. $])
            new SleepSorter(arg.to!uint).start;
}
