/**
 * Implementation of associative arrays.
 *
 * Copyright: Martin Nowak 2015 -.
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Martin Nowak
 */
module core.aa;

import core.memory : GC;

private
{
    // grow threshold
    enum GROW_NUM = 4;
    enum GROW_DEN = 5;
    // shrink threshold
    enum SHRINK_NUM = 1;
    enum SHRINK_DEN = 8;
    // grow factor
    enum GROW_FAC = 4;
    // growing the AA doubles it's size, so the shrink threshold must be
    // smaller than half the grow threshold to have a hysteresis
    static assert(GROW_FAC * SHRINK_NUM * GROW_DEN < GROW_NUM * SHRINK_DEN);
    // initial load factor (for literals), mean of both thresholds
    enum INIT_NUM = (GROW_DEN * SHRINK_NUM + GROW_NUM * SHRINK_DEN) / 2;
    enum INIT_DEN = SHRINK_DEN * GROW_DEN;

    // magic hash constants to distinguish empty, deleted, and filled buckets
    enum HASH_EMPTY = 0;
    enum HASH_DELETED = 0x1;
    enum HASH_FILLED_MARK = size_t(1) << 8 * size_t.sizeof - 1;
}

enum INIT_NUM_BUCKETS = 8;

struct AA(Key, Val)
{
    this(size_t sz)
    {
        impl = new Impl(nextpow2(sz));
    }

    @property bool empty() const pure nothrow @safe @nogc
    {
        return !length;
    }

    @property size_t length() const pure nothrow @safe @nogc
    {
        return impl is null ? 0 : impl.length;
    }

    void opIndexAssign(Val val, in Key key)
    {
        // lazily alloc implementation
        if (impl is null)
            impl = new Impl(INIT_NUM_BUCKETS);

        // get hash and bucket for key
        immutable hash = calcHash(key);

        // found a value => assignment
        if (auto p = impl.findSlotLookup(hash, key))
        {
            p.entry.val = val;
            return;
        }

        auto p = findSlotInsert(hash);
        if (p.deleted)
            --deleted;
        // check load factor and possibly grow
        else if (++used * GROW_DEN > dim * GROW_NUM)
        {
            grow();
            p = findSlotInsert(hash);
            assert(p.empty);
        }

        // update search cache and allocate entry
        firstUsed = min(firstUsed, cast(uint)(p - buckets.ptr));
        p.hash = hash;
        p.entry = new Impl.Entry(key, val); // TODO: move
        return;
    }

    ref inout(Val) opIndex(in Key key) inout @trusted
    {
        auto p = opIn_r(key);
        assert(p !is null);
        return *p;
    }

    inout(Val)* opIn_r(in Key key) inout @trusted
    {
        if (empty)
            return null;

        immutable hash = calcHash(key);
        if (auto p = findSlotLookup(hash, key))
            return &p.entry.val;
        return null;
    }

    bool remove(in Key key)
    {
        if (empty)
            return false;

        immutable hash = calcHash(key);
        if (auto p = findSlotLookup(hash, key))
        {
            // clear entry
            p.hash = HASH_DELETED;
            p.entry = null;

            ++deleted;
            if (length * SHRINK_DEN < dim * SHRINK_NUM)
                shrink();

            return true;
        }
        return false;
    }

    Val get(in Key key, lazy Val val)
    {
        auto p = opIn_r(key);
        return p is null ? val : *p;
    }

    ref Val getOrSet(in Key key, lazy Val val)
    {
        // lazily alloc implementation
        if (impl is null)
            impl = new Impl(INIT_NUM_BUCKETS);

        // get hash and bucket for key
        immutable hash = calcHash(key);

        // found a value => assignment
        if (auto p = impl.findSlotLookup(hash, key))
            return p.entry.val;

        auto p = findSlotInsert(hash);
        if (p.deleted)
            --deleted;
        // check load factor and possibly grow
        else if (++used * GROW_DEN > dim * GROW_NUM)
        {
            grow();
            p = findSlotInsert(hash);
            assert(p.empty);
        }

        // update search cache and allocate entry
        firstUsed = min(firstUsed, cast(uint)(p - buckets.ptr));
        p.hash = hash;
        p.entry = new Impl.Entry(key, val);
        return p.entry.val;
    }

    /**
       Convert the AA to the type of the builtin language AA.
     */
    Val[Key] toBuiltinAA() pure nothrow
    {
        return cast(Val[Key]) _aaFromCoreAA(impl, rtInterface);
    }

private:

    private this(inout(Impl)* impl) inout
    {
        this.impl = impl;
    }

    ref Val getLValue(in Key key)
    {
        // lazily alloc implementation
        if (impl is null)
            impl = new Impl(INIT_NUM_BUCKETS);

        // get hash and bucket for key
        immutable hash = calcHash(key);

        // found a value => assignment
        if (auto p = impl.findSlotLookup(hash, key))
            return p.entry.val;

        auto p = findSlotInsert(hash);
        if (p.deleted)
            --deleted;
        // check load factor and possibly grow
        else if (++used * GROW_DEN > dim * GROW_NUM)
        {
            grow();
            p = findSlotInsert(hash);
            assert(p.empty);
        }

        // update search cache and allocate entry
        firstUsed = min(firstUsed, cast(uint)(p - buckets.ptr));
        p.hash = hash;
        p.entry = new Impl.Entry(key); // TODO: move
        return p.entry.val;
    }

    static struct Impl
    {
        this(size_t sz)
        {
            buckets = allocBuckets(sz);
        }

        @property size_t length() const pure nothrow @nogc
        {
            assert(used >= deleted);
            return used - deleted;
        }

        @property size_t dim() const pure nothrow @nogc
        {
            return buckets.length;
        }

        @property size_t mask() const pure nothrow @nogc
        {
            return dim - 1;
        }

        // find the first slot to insert a value with hash
        inout(Bucket)* findSlotInsert(size_t hash) inout pure nothrow @nogc
        {
            for (size_t i = hash & mask, j = 1;; ++j)
            {
                if (!buckets[i].filled)
                    return &buckets[i];
                i = (i + j) & mask;
            }
        }

        // lookup a key
        inout(Bucket)* findSlotLookup(size_t hash, in Key key) inout
        {
            for (size_t i = hash & mask, j = 1;; ++j)
            {
                if (buckets[i].hash == hash && key == buckets[i].entry.key)
                    return &buckets[i];
                else if (buckets[i].empty)
                    return null;
                i = (i + j) & mask;
            }
        }

        void grow()
        {
            // If there are so many deleted entries, that growing would push us
            // below the shrink threshold, we just purge deleted entries instead.
            if (length * SHRINK_DEN < GROW_FAC * dim * SHRINK_NUM)
                resize(dim);
            else
                resize(GROW_FAC * dim);
        }

        void shrink()
        {
            if (dim > INIT_NUM_BUCKETS)
                resize(dim / GROW_FAC);
        }

        void resize(size_t ndim) pure nothrow
        {
            auto obuckets = buckets;
            buckets = allocBuckets(ndim);

            foreach (ref b; obuckets)
                if (b.filled)
                    *findSlotInsert(b.hash) = b;

            firstUsed = 0;
            used -= deleted;
            deleted = 0;
            GC.free(obuckets.ptr); // safe to free b/c impossible to reference
        }

        static struct Entry
        {
            Key key;
            Val val;
        }

        static struct Bucket
        {
            size_t hash;
            Entry* entry;

            @property bool empty() const
            {
                return hash == HASH_EMPTY;
            }

            @property bool deleted() const
            {
                return hash == HASH_DELETED;
            }

            @property bool filled() const
            {
                return cast(ptrdiff_t) hash < 0;
            }
        }

        Bucket[] allocBuckets(size_t dim) @trusted pure nothrow
        {
            enum attr = GC.BlkAttr.NO_INTERIOR;
            immutable sz = dim * Bucket.sizeof;
            return (cast(Bucket*) GC.calloc(sz, attr))[0 .. dim];
        }

        Bucket[] buckets;
        uint used;
        uint deleted;
        uint firstUsed;
    }

    RTInterface* rtInterface()() pure nothrow @nogc
    {
        static size_t aaLen(in void* pimpl) pure nothrow @nogc
        {
            auto aa = const(AA)(cast(const(Impl)*) pimpl);
            return aa.length;
        }

        static void* aaGetY(void** pimpl, in void* pkey)
        {
            auto aa = AA(cast(Impl*)*pimpl);
            auto res = &aa.getLValue(*cast(const(Key*)) pkey);
            *pimpl = aa.impl; // might have changed
            return res;
        }

        static inout(void)* aaInX(inout void* pimpl, in void* pkey)
        {
            auto aa = inout(AA)(cast(inout(Impl)*) pimpl);
            return aa.opIn_r(*cast(const(Key*)) pkey);
        }

        static bool aaDelX(void* pimpl, in void* pkey)
        {
            auto aa = AA(cast(Impl*) pimpl);
            return aa.remove(*cast(const(Key*)) pkey);
        }

        static immutable vtbl = RTInterface(&aaLen, &aaGetY, &aaInX, &aaDelX);
        return cast(RTInterface*)&vtbl;
    }

    static size_t calcHash(in ref Key key)
    {
        return hashOf(key) | HASH_FILLED_MARK;
    }

    Impl* impl;
    alias impl this;
}

package extern (C) void* _aaFromCoreAA(void* impl, RTInterface* rtIntf) pure nothrow;

private:

struct RTInterface
{
    alias AA = void*;

    size_t function(in AA aa) pure nothrow @nogc len;
    void* function(AA* aa, in void* pkey) getY;
    inout(void)* function(inout AA aa, in void* pkey) inX;
    bool function(AA aa, in void* pkey) delX;
}

unittest
{
    AA!(int, int) aa;
    assert(aa.length == 0);
    aa[0] = 1;
    assert(aa.length == 1 && aa[0] == 1);
    aa[1] = 2;
    assert(aa.length == 2 && aa[1] == 2);
    import core.stdc.stdio;

    int[int] rtaa = aa.toBuiltinAA();
    assert(rtaa.length == 2);
    puts("length");
    assert(rtaa[0] == 1);
    assert(rtaa[1] == 2);
    rtaa[2] = 3;

    assert(aa[2] == 3);
}

unittest
{
    auto aa = AA!(int, int)(3);
    aa[0] = 0;
    aa[1] = 1;
    aa[2] = 2;
    assert(aa.length == 3);
}

//==============================================================================
// Helper functions
//------------------------------------------------------------------------------

size_t nextpow2(in size_t n) pure nothrow @nogc
{
    import core.bitop : bsr;

    if (n < 2)
        return 1;
    return size_t(1) << bsr(n - 1) + 1;
}

pure nothrow @nogc unittest
{
    //                            0, 1, 2, 3, 4, 5, 6, 7, 8,  9
    foreach (const n, const pow2; [1, 1, 2, 4, 4, 8, 8, 8, 8, 16])
        assert(nextpow2(n) == pow2);
}

T min(T)(T a, T b) pure nothrow @nogc
{
    return a < b ? a : b;
}

T max(T)(T a, T b) pure nothrow @nogc
{
    return b < a ? a : b;
}
