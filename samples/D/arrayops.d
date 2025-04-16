/**
 * Benchmark for array ops.
 *
 * Copyright: Copyright Martin Nowak 2016 -.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Martin Nowak
 */
import core.cpuid, std.algorithm, std.datetime, std.meta, std.stdio, std.string,
    std.range;

float[6] getLatencies(T, string op)()
{
    enum N = (64 * (1 << 6) + 64) * T.sizeof;
    auto a = Array!T(N), b = Array!T(N), c = Array!T(N);
    float[6] latencies = float.max;
    foreach (i, ref latency; latencies)
    {
        auto len = 1 << i;
        foreach (_; 1 .. 32)
        {
            a[] = 24;
            b[] = 4;
            c[] = 2;
            auto sw = StopWatch(AutoStart.yes);
            foreach (off; size_t(0) .. size_t(64))
            {
                off = off * len + off;
                enum op = op.replace("const", "2").replace("a",
                        "a[off .. off + len]").replace("b",
                        "b[off .. off + len]").replace("c", "c[off .. off + len]");
                mixin(op ~ ";");
            }
            latency = min(latency, sw.peek.nsecs);
        }
    }
    float[6] res = latencies[] / 1024;
    return res;
}

float[4] getThroughput(T, string op)()
{
    enum N = (40 * 1024 * 1024 + 64 * T.sizeof) / T.sizeof;
    auto a = Array!T(N), b = Array!T(N), c = Array!T(N);
    float[4] latencies = float.max;
    size_t[4] lengths = [
        8 * 1024 / T.sizeof, 32 * 1024 / T.sizeof, 512 * 1024 / T.sizeof, 32 * 1024 * 1024 / T
        .sizeof
    ];
    foreach (i, ref latency; latencies)
    {
        auto len = lengths[i] / 64;
        foreach (_; 1 .. 4)
        {
            a[] = 24;
            b[] = 4;
            c[] = 2;
            auto sw = StopWatch(AutoStart.yes);
            foreach (off; size_t(0) .. size_t(64))
            {
                off = off * len + off;
                enum op = op.replace("const", "2").replace("a",
                        "a[off .. off + len]").replace("b",
                        "b[off .. off + len]").replace("c", "c[off .. off + len]");
                mixin(op ~ ";");
            }
            immutable nsecs = sw.peek.nsecs;
            runMasked({latency = min(latency, nsecs);});
        }
    }
    float[4] throughputs = void;
    runMasked({throughputs = T.sizeof * lengths[] / latencies[];});
    return throughputs;
}

string[] genOps()
{
    string[] ops;
    foreach (op1; ["+", "-", "*", "/"])
    {
        ops ~= "a " ~ op1 ~ "= b";
        ops ~= "a " ~ op1 ~ "= const";
        foreach (op2; ["+", "-", "*", "/"])
        {
            ops ~= "a " ~ op1 ~ "= b " ~ op2 ~ " c";
            ops ~= "a " ~ op1 ~ "= b " ~ op2 ~ " const";
        }
    }
    return ops;
}

void runOp(string op)()
{
    foreach (T; AliasSeq!(ubyte, ushort, uint, ulong, byte, short, int, long, float,
            double))
        writefln("%s, %s, %(%.2f, %), %(%s, %)", T.stringof, op,
            getLatencies!(T, op), getThroughput!(T, op));
}

struct Array(T)
{
    import core.stdc.stdlib : free, malloc;

    this(size_t n)
    {
        ary = (cast(T*) malloc(T.sizeof * n))[0 .. n];
    }

    ~this()
    {
        free(ary.ptr);
    }

    T[] ary;
    alias ary this;
}

version (X86)
    version = SSE;
else version (X86_64)
    version = SSE;
else
    static assert(0, "unimplemented");

version (SSE)
{
    uint mxcsr()
    {
        uint ret = void;
        asm
        {
            stmxcsr ret;
        }
        return ret;
    }

    void mxcsr(uint val)
    {
        asm
        {
            ldmxcsr val;
        }
    }

    // http://softpixel.com/~cwright/programming/simd/sse.php
    enum FPU_EXCEPTION_MASKS = 1 << 12 | 1 << 11 | 1 << 10 | 1 << 9 | 1 << 8 | 1 << 7;
    enum FPU_EXCEPTION_FLAGS = 1 << 5 | 1 << 4 | 1 << 3 | 1 << 2 | 1 << 1 | 1 << 0;

    void maskFPUExceptions()
    {
        mxcsr = mxcsr | FPU_EXCEPTION_MASKS;
    }

    void unmaskFPUExceptions()
    {
        mxcsr = mxcsr & ~FPU_EXCEPTION_MASKS;
    }

    uint FPUExceptionFlags()
    {
        return mxcsr & FPU_EXCEPTION_FLAGS;
    }

    void clearFPUExceptionFlags()
    {
        mxcsr = mxcsr & ~FPU_EXCEPTION_FLAGS;
    }
}

void runMasked(scope void delegate() dg)
{
    assert(FPUExceptionFlags == 0);
    maskFPUExceptions;
    dg();
    clearFPUExceptionFlags;
    unmaskFPUExceptions;
}

void main()
{
    unmaskFPUExceptions;

    writefln("type, op, %(latency%s, %), %-(throughput%s, %)", iota(6)
        .map!(i => 1 << i), ["8KB", "32KB", "512KB", "32MB"]);
    foreach (op; mixin("AliasSeq!(%(%s, %))".format(genOps)))
        runOp!op;
    maskFPUExceptions;
}
