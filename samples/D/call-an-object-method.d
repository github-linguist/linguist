struct Cat {
    static int staticMethod() {
        return 2;
    }

    string dynamicMethod() { // Never virtual.
        return "Mew!";
    }
}

class Dog {
    static int staticMethod() {
        return 5;
    }

    string dynamicMethod() { // Virtual method.
        return "Woof!";
    }
}

void main() {
    // Static methods calls:
    assert(Cat.staticMethod() == 2);
    assert(Dog.staticMethod() == 5);

    Cat c; // This is a value on the stack.
    Dog d; // This is just a reference, set to null.

    // Other static method calls, discouraged:
    assert(c.staticMethod() == 2);
    assert(d.staticMethod() == 5);

    // Instance method calls:
    assert(c.dynamicMethod() == "Mew!");
    d = new Dog;
    assert(d.dynamicMethod() == "Woof!");
}
