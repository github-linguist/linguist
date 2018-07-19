void main() {
    // Take the address of 'var' and placing it in a pointer:
    int var;
    int* ptr = &var;

    // Take the pointer to the first item of an array:
    int[10] data;
    auto p2 = data.ptr;


    // Depending on variable type, D will automatically pass either
    // by value or reference.
    // By value: structs, statically sized arrays, and other
    //           primitives (int, char, etc...);
    // By reference: classes;
    // By kind of reference: dynamically sized arrays, array slices.

    struct S {}
    class C {}

    void foo1(S s) {}        // By value.
    void foo2(C c) {}        // By reference.
    void foo3(int i) {}      // By value.
    void foo4(int[4] i) {}   // By value (unlike C).
    void foo6(int[] i) {}    // Just length-pointer struct by value.
    void foo5(T)(ref T t) {} // By reference regardless of what type
                             // T really is.
}
