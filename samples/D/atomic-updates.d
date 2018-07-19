import std.stdio: writeln;
import std.conv: text;
import std.random: uniform, Xorshift;
import std.algorithm: min, max;
import std.parallelism: task;
import core.thread: Thread;
import core.sync.mutex: Mutex;
import core.time: dur;

__gshared uint transfersCount;

final class Buckets(size_t nBuckets) if (nBuckets > 0) {
    alias TBucketValue = uint;

    // The trailing padding avoids cache line contention
    // when run with two or more cores.
    align(128) private static struct Bucket {
        TBucketValue value;
        Mutex mtx;
        alias value this;
    }

    private Bucket[nBuckets] buckets;
    private bool running;

    public this() {
        this.running = true;
        foreach (ref b; buckets)
            b = Bucket(uniform(0, 100), new Mutex);
    }

    public TBucketValue opIndex(in size_t index) const pure nothrow {
        return buckets[index];
    }

    public void transfer(in size_t from, in size_t to,
                         in TBucketValue amount) {
        immutable low  = min(from, to);
        immutable high = max(from, to);
        buckets[low].mtx.lock();
        buckets[high].mtx.lock();

        scope(exit) {
            buckets[low].mtx.unlock();
            buckets[high].mtx.unlock();
        }

        immutable realAmount = min(buckets[from].value, amount);
        buckets[from] -= realAmount;
        buckets[to  ] += realAmount;
        transfersCount++;
    }

    @property size_t length() const pure nothrow {
        return this.buckets.length;
    }

    void toString(in void delegate(const(char)[]) sink) {
        TBucketValue total = 0;
        foreach (ref b; buckets) {
            b.mtx.lock();
            total += b;
        }

        scope(exit)
            foreach (ref b; buckets)
                b.mtx.unlock();

        sink(text(buckets));
        sink(" ");
        sink(text(total));
    }
}

void randomize(size_t N)(Buckets!N data) {
    immutable maxi = data.length - 1;
    auto rng = Xorshift(1);

    while (data.running) {
        immutable i = uniform(0, maxi, rng);
        immutable j = uniform(0, maxi, rng);
        immutable amount = uniform(0, 20, rng);
        data.transfer(i, j, amount);
    }
}

void equalize(size_t N)(Buckets!N data) {
    immutable maxi = data.length - 1;
    auto rng = Xorshift(1);

    while (data.running) {
        immutable i = uniform(0, maxi, rng);
        immutable j = uniform(0, maxi, rng);
        immutable a = data[i];
        immutable b = data[j];
        if (a > b)
            data.transfer(i, j, (a - b) / 2);
        else
            data.transfer(j, i, (b - a) / 2);
    }
}

void display(size_t N)(Buckets!N data) {
    foreach (immutable _; 0 .. 10) {
        writeln(transfersCount, " ", data);
        transfersCount = 0;
        Thread.sleep(dur!"msecs"(1000));
    }
    data.running = false;
}

void main() {
    writeln("N. transfers, buckets, buckets sum:");
    auto data = new Buckets!20();
    task!randomize(data).executeInNewThread();
    task!equalize(data).executeInNewThread();
    task!display(data).executeInNewThread();
}
