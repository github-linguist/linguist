import std.stdio, std.conv, std.array, std.algorithm, std.numeric,
       std.range;

class M(T) {
    private size_t[] dim;
    private size_t[] subsize;
    private T[] d;

    this(size_t[] dimension ...) {
        setDimension(dimension);
        d[] = 0; // init each  entry to zero;
    }

    M!T dup() {
        M!T m = new M!T(dim);
        return m.set1DArray(d);
    }

    M!T setDimension(size_t[] dimension ...) {
        foreach (e; dimension)
            assert(e > 0, "no zero dimension");
        dim = dimension.dup;
        subsize = dim.dup;
        foreach (i; 0 .. dim.length)
            subsize[i] = reduce!"a*b"(1, dim[i + 1 .. $]);
        auto dlength = dim[0] * subsize[0];
        if (d.length != dlength)
            d = new T[dlength];
        return this;
    }

    M!T set1DArray(T[] t ...) {
        auto minLen = min(t.length, d.length);
        d[] = 0;
        d[0 .. minLen] = t[0 .. minLen];
        return this;
    }

    size_t[] seq2idx(size_t seq) {
        size_t acc = seq, tmp;
        size_t[] idx;
        foreach (e; subsize) {
            idx ~= tmp = acc / e;
            acc = acc - tmp * e; // same as % (mod) e;
        }
        return idx;
    }

    size_t size() const @property { return d.length; }

    size_t rank() const @property { return dim.length; }

    size_t[] shape() const @property { return dim.dup; }

    T[] raw() const @property { return d.dup; }

    bool checkBound(size_t[] idx ...) const {
        if (idx.length > dim.length)
            return false;
        foreach (i, dm; idx)
            if (dm >= dim[i])
                return false;
        return true;
    }

    T opIndex(size_t[] idx ...) {
        assert(checkBound(idx), "OOPS");
        return d[dotProduct(idx, subsize)];
    }

    T opIndexAssign(T v, size_t[] idx ...) {
        assert(checkBound(idx), "OOPS");
        d[dotProduct(idx, subsize)] = v;
        return v;
    }

    override bool opEquals(Object o) {
        auto rhs = to!(M!T)(o);
        return dim == rhs.dim && d == rhs.d;
    }

    int opApply(int delegate(ref size_t[]) dg) {
        size_t[] yieldIdx;
        foreach (i; 0 .. d.length) {
            yieldIdx = seq2idx(i);
            if (dg(yieldIdx))
                break;
        }
        return 0;
    }

    int opApply(int delegate(ref size_t[], ref T) dg) {
        size_t idx1d = 0;
        foreach (size_t[] idx; this) {
            if (dg(idx, d[idx1d++]))
                break;
        }
        return 0;
    }

    M!T convolute(M!T rhs) { // _this_ is h, rhs is f, output g
        auto dm = dim.dup;
        dm[] += rhs.dim[] - 1;
        M!T m = new M!T(dm); // dm will be reused as m's idx
        auto bound = m.size;
        foreach (i; 0 .. d.length) {
            auto thisIdx = seq2idx(i);
            foreach (j; 0 .. rhs.d.length) {
                dm[] = thisIdx[] + rhs.seq2idx(j)[];
                auto midx1d = dotProduct(dm, m.subsize);
                if ( midx1d < bound)
                    m.d[midx1d] += d[i] * rhs.d[j];
                else
                    break; // bound reach, ok to break
            }
        }
        return m;
    }

    M!T deconvolute(M!T rhs) { // _this_ is g, rhs is f, output is h
        auto dm = dim.dup;
        foreach (i, e; dm)
            assert(e + 1 > rhs.dim[i],
                   "deconv : dimensions is zero or negative");
        dm[] -= (rhs.dim[] - 1);
        M!T m = new M!T(dm); // dm will be reused as rhs' idx

        foreach (i; 0 .. m.size) {
            auto idx = m.seq2idx(i);
            m.d[i] = this[idx];
            foreach (j; 0 .. i) {
                auto jdx = m.seq2idx(j);
                dm[] = idx[] - jdx[];
                if (rhs.checkBound(dm))
                    m.d[i] -=  m.d[j] * rhs[dm];
            }
            m.d[i] /=  rhs.d[0];
        }
        return m;
    }

    override string toString() { return to!string(d); }
}

auto fold(T)(T[] arr, ref size_t[] d) {
    if (d.length == 0)
        d ~= arr.length;
    static if (is(T U : U[])) { // is arr an array of arrays?
        assert(arr.length > 0, "no empty dimension");
        d ~= arr[0].length;
        foreach (e; arr)
            assert(e.length == arr[0].length, "not rectangular");
        return fold(reduce!q{a ~ b}(arr), d);
    } else {
        assert(arr.length == reduce!q{a * b}(d), "not same size");
        return arr;
    }
}

auto arr2M(T)(T a) {
    size_t[] dm;
    auto d = fold(a, dm);
    alias ElementType!(typeof(d)) E;
    auto m = new M!E(dm);
    m.set1DArray(d);
    return m;
}

void main() {
    alias M!int Mi;
    auto hh = [[[-6, -8, -5, 9], [-7, 9, -6, -8], [2, -7, 9, 8]],
               [[7, 4, 4, -6], [9, 9, 4, -4], [-3, 7, -2, -3]]];
    auto ff = [[[-9, 5, -8], [3, 5, 1]],[[-1, -7, 2], [-5, -6, 6]],
               [[8, 5, 8],[-2, -6, -4]]];
    auto h = arr2M(hh);
    auto f = arr2M(ff);

    auto g = h.convolute(f);

    writeln("g == f convolute h ? ", g == f.convolute(h));
    writeln("h == g deconv f    ? ", h == g.deconvolute(f));
    writeln("f == g deconv h    ? ", f == g.deconvolute(h));
    writeln("         f = ", f);
    writeln("g deconv h = ", g.deconvolute(h));
}
