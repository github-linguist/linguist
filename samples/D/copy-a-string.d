void main() {
    string src = "This is a string";

    // copy contents:
    auto dest1 = src.idup;

    // copy contents to mutable char array
    auto dest2 = src.dup;

    // copy just the fat reference of the string
    auto dest3 = src;
}
