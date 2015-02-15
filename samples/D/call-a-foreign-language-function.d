import std.stdio: writeln;
import std.string: toStringz;
import std.conv: to;

extern(C) {
    char* strdup(in char* s1);
    void free(void* ptr);
}

void main() {
    // We could use char* here (as in D string literals are
    // null-terminated) but we want to comply with the "of the
    // string type typical to the language" part.
    // Note: D supports 0-values inside a string, C doesn't.
    auto input = "Hello World!";

    // Method 1 (preferred):
    //   toStringz converts D strings to null-terminated C strings.
    char* str1 = strdup(toStringz(input));

    // Method 2:
    // D strings are not null-terminated, so we append '\0'.
    // .ptr returns a pointer to the 1st element of the array,
    // just as &array[0]
    // This has to be done because D dynamic arrays are
    // represented with:  { size_t length; T* pointer; }
    char* str2 = strdup((input ~ '\0').ptr);

    // We could have just used printf here, but the task asks to
    // "print it using language means":
    writeln("str1: ", to!string(str1));
    writeln("str2: ", to!string(str2));

    free(str1);
    free(str2);
}
