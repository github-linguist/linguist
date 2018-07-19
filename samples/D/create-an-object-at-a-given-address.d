import std.stdio ;

void main() {
    int[] arr ;
    foreach(i; [0,1,2,3])
        arr ~= i*(1 << 24) + 0x417e7e7e ;

    struct X {
        char[16] msg ;
    }

    X* xPtr ;
    int* iPtr ;
    float* fPtr ;

    int adrSpace = cast(int) arr.ptr ;
    // get address of an existing object arr

    xPtr = cast(X*) adrSpace ;
    // xPtr now point to arr, as a struct X
    writefln("arr(as X)'s msg = '%s' (len %d) @ 0x%08x",
        xPtr.msg, xPtr.msg.length, xPtr) ;

    iPtr = cast(int*) (adrSpace + 1 * 4 /*bytes*/) ;
    fPtr = cast(float*) iPtr ;
    // pointers now point to arr[1]
    writefln("arr[1] = 0x%8x (%9.4f) @ 0x%08X", *iPtr, *fPtr, iPtr) ;
    iPtr = cast(int*) (adrSpace + 3 * 4 /*bytes*/) ;
    fPtr = cast(float*) iPtr ;
    // pointers now point to arr[3]
    writefln("arr[3] = 0x%8x (%9.4f) @ 0x%08X", *iPtr, *fPtr, iPtr) ;
    *fPtr = 0.5f ; // change value
    writefln("arr[3] = 0x%8x (%9.4f) @ 0x%08X", *iPtr, *fPtr, iPtr) ;
}
