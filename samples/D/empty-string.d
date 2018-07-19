import std.array;

bool isEmptyNotNull(in string s) pure nothrow @safe {
    return s is "";
}

void main(){
    string s1 = null;
    string s2 = "";

    // the content is the same
    assert(!s1.length);
    assert(!s2.length);
    assert(s1 == "" && s1 == null);
    assert(s2 == "" && s2 == null);
    assert(s1 == s2);

    // but they don't point to the same memory region
    assert(s1 is null && s1 !is "");
    assert(s2 is "" && s2 !is null);
    assert(s1 !is s2);
    assert(s1.ptr == null);
    assert(*s2.ptr == '\0'); // D string literals are \0 terminated

    assert(s1.empty);
    assert(s2.isEmptyNotNull());
}
