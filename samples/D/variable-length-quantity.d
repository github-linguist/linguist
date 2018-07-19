import std.stdio, std.string, std.file, std.algorithm;

/// Variable length quantity (unsigned long, max 63-bit).
struct VLQ {
    ulong value;

    // This allows VLQ to work like an ulong.
    alias value this;

    uint extract(in ubyte[] v) pure
    in {
        assert(v.length > 0);
    } body {
        immutable limit = min(v.length - 1, 8);
        ulong t = 0;
        size_t idx = 0;
        while ((idx < limit) && ((v[idx] & 0x80) > 0))
            t = (t << 7) | (0x7f & v[idx++]);
        if (idx > limit)
            throw new Exception(
                "Too large for ulong or invalid format.");
        else
            value = (t << 7) | v[idx];
        return idx + 1;
    }

    VLQ from(in ubyte[] v) pure {
        extract(v);
        return this;
    }

    @property ubyte[] toVLQ() const pure {
        ubyte[] v = [0x7f & value];
        for (ulong k = value >>> 7; k > 0; k >>>= 7)
            v ~= (k & 0x7f) | 0x80;
        if (v.length > 9)
            throw new Exception("Too large value.");
        v.reverse();
        return v;
    }

    static ulong[] split(in ubyte[] b) pure {
        ulong[] res;
        VLQ v;
        for (size_t i = 0; i < b.length; ) {
            i += v.extract(b[i .. $]);
            res ~= v.value;
        }
        return res;
    }

    string toString() const pure /*nothrow*/ {
        return format("(%(%02X:%))", this.toVLQ);
    }
}


void main() { // VLQ demo code.
    VLQ a = VLQ(0x7f),
        b = VLQ(0x4000),
        c;
    writefln("a:%8x = %s\nb:%8x = %s\nc:%8x = %s",
             a.value, a, b.value, b, c.value, c);

    // Some operations.
    c = (a + 1) * b;
    a = c - 1;
    b = VLQ().from(a.toVLQ);
    a <<= 1;

    // Convert ulong to octet sequence.
    writefln("\na:%8x = %s\nb:%8x = %s\nc:%8x = %s",
             a.value, a, b.value, b, c.value, c);

    // Write them to a binary file.
    std.file.write("vlqtest.bin", a.toVLQ ~ b.toVLQ ~ c.toVLQ);

    // Read them back.
    const buf = cast(ubyte[])std.file.read("vlqtest.bin");
    writefln("\nFile length: %d bytes.", buf.length);

    // Convert octet sequence to ulongs.
    foreach (immutable i, immutable v; VLQ.split(buf))
        writefln("%d:%8x = %s", i + 1, v, VLQ(v));
}
