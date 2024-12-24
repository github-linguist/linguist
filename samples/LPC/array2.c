string* arr = ({ "a", "b", "c" });
test() {
    arr += ({ "d" });
    // should always allow subtracting 0 elements
    arr -= ({ 0 });
    arr -= ({ "a" });
}