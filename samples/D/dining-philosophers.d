import std.stdio, std.algorithm, std.string, std.parallelism,
       core.sync.mutex;

void eat(in size_t i, in string name, Mutex[] forks) {
    writeln(name, " is hungry.");
    immutable j = (i + 1) % forks.length;

    // Take forks i and j. The lower one first to prevent deadlock.
    auto fork1 = forks[min(i, j)];
    auto fork2 = forks[max(i, j)];

    fork1.lock;
    scope(exit) fork1.unlock;

    fork2.lock;
    scope(exit) fork2.unlock;

    writeln(name, " is eating.");
    writeln(name, " is full.");
}

void think(in string name) {
    writeln(name, " is thinking.");
}

void main() {
    const philosophers = "Aristotle Kant Spinoza Marx Russell".split;
    Mutex[philosophers.length] forks;
    foreach (ref fork; forks)
        fork = new Mutex;

    defaultPoolThreads = forks.length;
    foreach (i, philo; taskPool.parallel(philosophers)) {
        foreach (immutable _; 0 .. 100) {
            eat(i, philo, forks);
            philo.think;
        }
    }
}
