import std.stdio, std.array, std.datetime, std.exception,
       std.concurrency, core.thread, core.atomic;

final class OutOfInk: Exception {
    this() pure nothrow {
        super("Out of ink.");
    }
}

struct Printer {
    string id;
    size_t ink;

    void printIt(in string line) {
        enforce(ink != 0, new OutOfInk);
        writefln("%s: %s", id, line);
        ink--;
    }
}

/// Treats shared lvalue as if it is thread-local.
ref assumeThreadLocal(T)(ref shared T what) pure nothrow {
    return *cast(T*)&what;
}

struct RendezvousPrinter {
    Printer[] printers;

    void print(const(string)[] lines) shared {
        OutOfInk savedException;

        // Lightweight mutual exclusion
        // using shared atomic bool.
        shared static bool mutex;

        void lock() {
            while (!cas(&mutex, false, true))
                Thread.sleep(1.hnsecs);
        }

        void unlock() nothrow {
            assert(mutex.atomicLoad!(MemoryOrder.acq));
            mutex.atomicStore!(MemoryOrder.rel)(false);
        }

        while (!lines.empty) {
            if (printers.empty) {
                // No more printers to try.
                assert(savedException !is null);
                throw savedException;
            }

            try {
                {
                    lock;
                    scope(exit) unlock;
                    printers.front.assumeThreadLocal
                        .printIt(lines.front);
                }
                lines.popFront;

                // Increase the chance of interleaved output.
                Thread.sleep(10.msecs);
            } catch (OutOfInk exc) {
                savedException = exc;

                // Switch to the next printer.
                lock;
                scope(exit) unlock;
                printers.assumeThreadLocal.popFront;
            }
        }
    }
}

void humptyDumptyTask(shared ref RendezvousPrinter rendezvous) {
    const humptyDumpty = [
        "Humpty Dumpty sat on a wall.",
        "Humpty Dumpty had a great fall.",
        "All the king's horses and all the king's men,",
        "Couldn't put Humpty together again."];

    rendezvous.print(humptyDumpty);
}

void motherGooseTask(shared ref RendezvousPrinter rendezvous) {
    const motherGoose = ["Old Mother Goose,",
                         "When she wanted to wander,",
                         "Would ride through the air,",
                         "On a very fine gander.",
                         "Jack's mother came in,",
                         "And caught the goose soon,",
                         "And mounting its back,",
                         "Flew up to the moon."];

    rendezvous.print(motherGoose);
}

void main() {
    shared rendezvous = RendezvousPrinter
        ([Printer("main", 5), Printer("reserve", 5)]);

    spawn(&humptyDumptyTask, rendezvous);
    spawn(&motherGooseTask, rendezvous);
}
