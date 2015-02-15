import std.stdio, core.thread;

void main() {
    write("Enter a time to sleep (in seconds): ");

    long secs;
    readf(" %d", &secs);

    writeln("Sleeping...");
    Thread.sleep(dur!"seconds"(secs));
    writeln("Awake!");
}
