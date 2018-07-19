import std.stdio, core.thread, std.datetime;

class NauticalBell : Thread {
    private shared bool stopped;

    this() {
        super(&run);
    }

    void run() {
        uint numBells;
        auto time = cast(TimeOfDay)Clock.currTime();
        auto next = TimeOfDay();

        void setNextBellTime() {
            next += minutes(30);
            numBells = 1 + (numBells % 8);
        }

        while (next < time)
            setNextBellTime();

        while (!this.stopped) {
            time = cast(TimeOfDay)Clock.currTime();
            if (next.minute == time.minute &&
                    next.hour == time.hour) {
                immutable bells = numBells == 1 ? "bell" : "bells";
                writefln("%s : %d %s", time, numBells, bells);
                setNextBellTime();
            }
            sleep(dur!"msecs"(100));
            yield();
         }
     }

     void stop() {
        this.stopped = true;
     }
}

void main() {
    auto bells = new NauticalBell();
    bells.isDaemon(true);
    bells.start();
    try {
        bells.join();
    } catch (ThreadException e) {
        writeln(e.msg);
    }
}
