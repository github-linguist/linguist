// D is a system language so its memory management is refined.
// D supports thread-local memory on default, global memory, memory
// allocated on the stack, the C heap, or the D heap managed by a
// garbage collector, both manually and automatically.

// This program looks scary because its purpose is to show all the
// variety. But lot of this stuff is only for special situations
// (like alloca), and it's not necessary in most user code.

enum int nInts = 10; // Compile-time constant.

// This is thread-local:
int data1[nInts];

// This is global:
__gshared int data2[nInts];

void main() {
    // Static memory, it's thread-local but its name is usable
    // only locally:
    static int data3[nInts];

    // Static memory, it's global but its name is usable only locally:
    __gshared static int data4[nInts];

    // ----------------------
    // D supports the functions that manage memory of the C heap:
    import core.stdc.stdlib: malloc, calloc, realloc, free, alloca;

    // Allocates space for some integers on the heap,
    // the memory is not initialized:
    auto ptr1 = cast(int*)malloc(nInts * int.sizeof);
    if (ptr1 == null)
        return;

    // Increases the space for one more integer, the new space
    // is not initialized, but the old space is not modified:
    ptr1 = cast(int*)realloc(ptr1, (nInts + 1) * int.sizeof);
    if (ptr1 == null)
        return;

    // calloc allocates on the heap and zeros the memory:
    auto ptr2 = cast(int*)calloc(nInts, int.sizeof);
    if (ptr2 == null)
        return;

    // You can create a slice from a pointer:
    auto slice1 = ptr2[0 .. nInts];

    // Frees the memory:
    free(ptr2);
    free(ptr1);

    // ----------------------
    import core.stdc.stdio: puts;

    static struct Test {
        ~this() { puts("Test destructor"); }
    }

    // Memory allocated on the stack:
    Test[2] array1;

    {
        // More memory allocated on the stack:
        Test[2] array2;
        // Here array2 is removed from the stack,
        // and all array2 destructors get called.
    }
    puts("Block end.");

    // alloca is supported in D. It's similar to malloc but the
    // memory is allocated on the stack:
    int* ptr3 = cast(int*)alloca(nInts * int.sizeof);

    // You can create a slice from the pointer:
    auto slice2 = ptr3[0 .. nInts];

    // Do not free the memory allocated with alloca:
    // free(ptr3);

    // ----------------------
    // Allocates a dynamic array on the D heap managed by
    // the D garbage collector:
    auto array3 = new int[nInts];

    // Try to reserve capacity for a dynamic array on the D heap:
    int[] array4;
    array4.reserve(nInts);
    assert(array4.capacity >= nInts);
    assert(array4.length == 0);

    // Appends one integer to the dynamic array:
    array4 ~= 100;

    // Assume that it is safe to append to this array. Appends made
    // to this array after calling this function may append in place,
    // even if the array was a slice of a larger array to begin with:
    array4.assumeSafeAppend;
    array4 ~= 200;
    array4 ~= 300;
    assert(array4.length == 3);
    // See here for more info:
    // http://dlang.org/d-array-article.html


    // Allocates a struct and a class on the D GC heap:
    static class Foo { int x; }
    Test* t = new Test; // This destructor will not be called.
    Foo f1 = new Foo; // f1 is a class reference.

    // Optional. Destroys the given object and puts it in
    // an invalid state:
    f1.destroy;

    import std.typecons: scoped;

    // Allocates a class on the stack, unsafe:
    auto f3 = scoped!Foo();

    // ----------------------
    import core.memory: GC;

    // Allocates an aligned block from the GC, initialized to zero.
    // Plus it doesn't scan through this block on collect.
    auto ptr4 = cast(int*)GC.calloc(nInts * int.sizeof,
                                    GC.BlkAttr.NO_SCAN);

    // No need to test for this, because GC.calloc usually
    // throws OutOfMemoryError if it can't allocate.
    // if (ptr4 == null)
    //    exit(1);

    GC.free(ptr4); // This is optional.
}
