import std.random;

// enum allows to define manifest (compile-time) constants:
int sqr(int x) { return x ^^ 2; }
enum int x = 5;
enum y = sqr(5); // Forces Compile-Time Function Evaluation (CTFE).

// enums are compile-time constants:
enum MyEnum { A, B, C }


// immutable defines values that can't change:
immutable double pi = 3.1415;


// A module-level immutable storage class variable that's not
// explicitly initialized can be initialized by its constructor,
// otherwise its value is the default initializer during its life-time.

immutable int z;

static this() {
    z = uniform(0, 100); // Run-time initialization.
}

class Test1 {
    immutable int w;

    this() {
        w = uniform(0, 100); // Run-time initialization.
    }
}


// The items array can't be immutable here.
// "in" is short for "const scope":
void foo(const scope int[] items) {
    // items is constant here.
    // items[0] = 100; // Cannot modify const expression.
}


struct Test2 {
    int x_; // Mutable.
    @property int x() { return this.x_; }
}

// Unlike C++, D const and immutable are transitive.
// And there is also "inout". See D docs.

void main() {
    int[] data = [10, 20, 30];
    foo(data);
    data[0] = 100; // But data is mutable here.

    // Currently manifest constants like arrays and associative arrays
    // are copied in-place every time they are used:
    enum array = [1, 2, 3];
    foo(array);

    auto t = Test2(100);
    auto x2 = t.x; // Reading x is allowed.
    assert(x2 == 100);

    // Not allowed, the setter property is missing:
    // t.x = 10; // Error: not a property t.x
}
