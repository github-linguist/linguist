import std.stdio, std.random, std.algorithm, std.range, std.ascii;

enum target = "METHINKS IT IS LIKE A WEASEL"d;
enum C = 100;  // Number of children in each generation.
enum P = 0.05; // Mutation probability.
enum fitness = (dchar[] s) => target.zip(s).count!q{ a[0] != a[1] };
dchar rnd() { return (uppercase ~ " ")[uniform(0, $)]; }
enum mut= (dchar[] s) => s.map!(a=> uniform(0,1.) < P ? rnd : a).array;

void main() {
    auto parent = target.length.iota.map!(_ => rnd).array;
    for (auto gen = 1; parent != target; gen++) {
        // parent = parent.repeat(C).map!mut.array.max!fitness;
        parent = parent.repeat(C).map!mut.array
                 .minPos!((a, b) => a.fitness < b.fitness)[0];
        writefln("Gen %2d, dist=%2d: %s", gen, parent.fitness, parent);
    }
}
