import std.bitmanip, core.stdc.string, std.conv, std.math, std.array,
       std.string;

version (D_InlineAsm_X86) {} else {
    static assert(false, "For X86 machine only.");
}

// CTFE construction of transform expressions.
uint S(in uint n) pure nothrow {
    return [7u, 12, 17, 22, 5, 9, 14, 20, 4, 11, 16, 23, 6, 10, 15, 21]
           [(n / 16) * 4 + (n % 4)];
}

uint K(in uint n) pure nothrow {
    uint r = 0;
    if (n <= 15)
        r = n;
    else if (n <= 31)
        r = 5 * n + 1;
    else if (n <= 47)
        r = 3 * n + 5;
    else
        r = 7 * n;
    return r % 16;
}

uint T(in uint n) pure nothrow {
    return cast(uint)(abs(sin(n + 1.0L)) * (2UL ^^ 32));
}

string[] ABCD(in int n) pure nothrow {
    enum abcd = ["EAX", "EBX", "ECX", "EDX"];
    return abcd[(64 - n) % 4 .. 4] ~ abcd[0 .. (64 - n) % 4];
}

string SUB(in int n, in string s) pure nothrow {
    return s
           .replace("ax", n.ABCD[0])
           .replace("bx", n.ABCD[1])
           .replace("cx", n.ABCD[2])
           .replace("dx", n.ABCD[3]);
}

// FF, GG, HH & II expressions part 1 (F, G, H, I).
string fghi1(in int n) pure nothrow {
    switch (n / 16) {
        case 0:
            // (bb & cc) | (~bb & dd)
            return q{
                        mov ESI, bx;
                        mov EDI, bx;
                        not ESI;
                        and EDI, cx;
                        and ESI, dx;
                        or EDI, ESI;
                        add ax, EDI;
                    };
        case 1:
            // (dd & bb) | (~dd & cc)
            return q{
                        mov ESI, dx;
                        mov EDI, dx;
                        not ESI;
                        and EDI, bx;
                        and ESI, cx;
                        or EDI, ESI;
                        add ax, EDI;
                    };
        case 2: // (bb ^ cc ^ dd)
            return q{
                        mov EDI, bx;
                        xor EDI, cx;
                        xor EDI, dx;
                        add ax, EDI;
                    };
        case 3: // (cc ^ (bb | ~dd))
            return q{
                       mov EDI, dx;
                       not EDI;
                       or EDI, bx;
                       xor EDI, cx;
                       add ax, EDI;
                    };
        default:
            assert(false);
    }
}

// FF, GG, HH & II expressions part 2.
string fghi2(in int n) pure nothrow {
    return q{
                add ax, [EBP + 4 * KK];
                add ax, TT;
            } ~ n.fghi1;
}

// FF, GG, HH & II expressions prepended with previous parts
// & subsitute ABCD.
string FGHI(in int n) pure nothrow {
    // aa = ((aa << SS)|( aa >>> (32 - SS))) + bb = ROL(aa, SS) + bb
    return SUB(n, n.fghi2 ~ q{
                                rol ax, SS;
                                add ax, bx;
                             });
}

string genExpr(uint n) pure {
    return FGHI(n)
           .replace("SS", n.S.text)
           .replace("KK", n.K.text)
           .replace("TT", "0x" ~ to!string(n.T, 16));
}

string genTransformCode(int n) pure {
    return (n < 63) ? n.genExpr ~ genTransformCode(n + 1) : n.genExpr;
}

enum string coreZMD5 = 0.genTransformCode;

struct ZMD5 {
    uint[4] state = [0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476];
    ulong count;
    ubyte[64] buffer;

    ubyte[64] padding = [
      0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0];

    private void transform(ubyte* block) pure nothrow {
        uint[16] x = void;

        version (BigEndian) {
            foreach (immutable size_t i; 0 .. 16)
                x[i] = littleEndianToNative!uint(
                            *cast(ubyte[4]*)&block[i * 4]);
        } else {
            (cast(ubyte*)x.ptr)[0 .. 64] = block[0 .. 64];
        }

        auto pState = state.ptr;
        auto pBuffer = x.ptr;

        asm {
            mov  ESI, pState[EBP];
            mov  EDX, [ESI + 3 * 4];
            mov  ECX, [ESI + 2 * 4];
            mov  EBX, [ESI + 1 * 4];
            mov  EAX, [ESI + 0 * 4];
            push EBP;
            push ESI;

            mov  EBP, pBuffer[EBP];
        }

        mixin("asm { " ~ coreZMD5 ~ "}");

        asm {
            pop ESI;
            pop EBP;
            add [ESI + 0 * 4], EAX;
            add [ESI + 1 * 4], EBX;
            add [ESI + 2 * 4], ECX;
            add [ESI + 3 * 4], EDX;
        }
        x[] = 0;
    }

    void update(in void[] input) pure nothrow {
        auto inputLen = input.length;
        uint index = (cast(uint)count >> 3) & (64 - 1);
        count += inputLen * 8;
        immutable uint partLen = 64 - index;

        uint i;
        if (inputLen >= partLen) {
            memcpy(&buffer[index], input.ptr, partLen);
            transform(buffer.ptr);
            for (i = partLen; i + 63 < inputLen; i += 64)
                transform((cast(ubyte[])input)[i .. i + 64].ptr);
            index = 0;
        } else
            i = 0;

        if (inputLen - i)
            memcpy(&buffer[index], &input[i], inputLen - i);
    }

    void finish(ref ubyte[16] digest) pure nothrow {
        ubyte bits[8] = void;
        bits[0 .. 8] = nativeToLittleEndian(count)[];

        immutable uint index = (cast(uint)count >> 3) & (64 - 1);
        immutable uint padLen = (index < 56) ?
                                (56 - index) : (120 - index);
        update(padding[0 .. padLen]);
        update(bits);

        digest[0 .. 4]   = nativeToLittleEndian(state[0])[];
        digest[4 .. 8]   = nativeToLittleEndian(state[1])[];
        digest[8 .. 12]  = nativeToLittleEndian(state[2])[];
        digest[12 .. 16] = nativeToLittleEndian(state[3])[];

        // Zeroize sensitive information.
        memset(&this, 0, ZMD5.sizeof);
    }
}

string getDigestString(in void[][] data...) pure {
    ZMD5 ctx;
    foreach (datum; data)
        ctx.update(datum);
    ubyte[16] digest;
    ctx.finish(digest);
    return format("%-(%02X%)", digest);
}


void main() { // Benchmark code --------------
    import std.stdio, std.datetime, std.digest.md;

    writefln(`md5  digest("")  = %-(%02X%)`, "".md5Of);
    writefln(`zmd5 digest("")  = %s`, "".getDigestString);

    enum megaBytes = 512;
    writefln("\nTest performance / message size %dMBytes:", megaBytes);
    auto data = new float[megaBytes * 0x40000 + 13];

    StopWatch sw;
    sw.start;
    immutable d1 = data.md5Of;
    sw.stop;
    immutable time1 = sw.peek.msecs / 1000.0;
    writefln("digest(data) = %-(%02X%)", d1);
    writefln("std.md5: %8.2f M/sec  ( %8.2f secs)",
             megaBytes / time1, time1);

    sw.reset;
    sw.start;
    immutable d2 = data.getDigestString;
    sw.stop;
    immutable time2 = sw.peek.msecs / 1000.0;
    writefln("digest(data) = %s", d2);
    writefln("zmd5   : %8.2f M/sec  ( %8.2f secs)",
             megaBytes / time2, time2);
}
