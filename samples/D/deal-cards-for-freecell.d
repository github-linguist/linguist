import std.stdio, std.conv, std.algorithm, std.range;

struct RandomGenerator {
    uint seed = 1;

    @property uint next() pure nothrow {
        seed = (seed * 214_013 + 2_531_011) & int.max;
        return seed >> 16;
    }
}

struct Deck {
    int[52] cards;

    void deal(in uint seed) pure /*nothrow*/ {
        enum int nc = cards.length; // Must be signed for iota.
        iota(nc - 1, -1, -1).copy(cards[]); // iota isn't nothrow.

        auto rnd = RandomGenerator(seed);
        foreach (immutable i, ref c; cards)
            c.swap(cards[(nc - 1) - rnd.next % (nc - i)]);
    }

    void show() const {
        writefln("%(%-( %s%)\n%)",
                 cards[]
                 .chunks(8)
                 .map!(row => row.map!(c => ["A23456789TJQK"[c / 4]] ~
                                            "CDHS"[c % 4])));
    }
}

void main(in string[] args) {
    immutable seed = (args.length == 2) ? args[1].to!uint : 11_982;
    writeln("Hand ", seed);
    Deck cards;
    cards.deal(seed);
    cards.show;
}
