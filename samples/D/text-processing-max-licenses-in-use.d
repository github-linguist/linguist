void main() {
    import std.stdio;
    int nOut, maxOut = -1;
    string[] maxTimes;

    foreach (string job; lines(File("mlijobs.txt"))) {
        nOut += (job[8] == 'O') ? 1 : -1;
        if (nOut > maxOut) {
            maxOut = nOut;
            maxTimes = null;
        }
        if (nOut == maxOut)
            maxTimes ~= job[14 .. 33];
    }

    writefln("Maximum simultaneous license use is %d at" ~
             " the following times:\n%( %s\n%)", maxOut, maxTimes);
}
