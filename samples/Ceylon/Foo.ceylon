"Test function for Ceylon"
by ("Enrique")
shared void test() {
    print("test");
}

"Test class for Ceylon"
shared class Test(name) satisfies Comparable<Test> {
    shared String name;
    shared actual String string = "Test ``name``.";

    shared actual Comparison compare(Test other) {
        return name<=>other.name;
    }
}
