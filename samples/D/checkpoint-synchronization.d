import std.stdio;
import std.parallelism: taskPool, defaultPoolThreads, totalCPUs;

void buildMechanism(uint nparts) {
    auto details = new uint[nparts];
    foreach (i, ref detail; taskPool.parallel(details)) {
        writeln("Build detail ", i);
        detail = i;
    }

    // This could be written more concisely via std.parallelism.reduce,
    // but we want to see the checkpoint explicitly.
    writeln("Checkpoint reached. Assemble details ...");
    uint sum = 0;
    foreach (immutable detail; details)
        sum += detail;
    writeln("Mechanism with ", nparts, " parts finished: ", sum);
}

void main() {
    defaultPoolThreads = totalCPUs + 1; // totalCPUs - 1 on default.
    buildMechanism(42);
    buildMechanism(11);
}
