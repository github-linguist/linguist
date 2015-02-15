// Named (commonly used enum in D) (int).
enum Fruits1 { apple, banana, cherry }

// Anonymous, as in C.
enum { APPLE, BANANA, CHERRY }

// Named with specified values (int).
enum Fruits2 { apple = 0, banana = 10, cherry = 20 }

// Named, typed and with specified values.
enum Fruits3 : ubyte { apple = 0, banana = 100, cherry = 200 }

void main() {
    static assert(CHERRY == 2);
    int f1 = Fruits2.banana; // No error.
    // Fruits2 f2 = 1; // Error: cannot implicitly convert.
}
