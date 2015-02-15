import std.stdio;

class MyClass {
    //constructor (not necessary if empty)
    this() {}

    void someMethod() {
        variable = 1;
    }

    // getter method
    @property int variable() const {
        return variable_;
    }

    // setter method
    @property int variable(int newVariable) {
        return variable_ = newVariable;
    }

    private int variable_;
}

void main() {
    // On default class instances are allocated on the heap
    // The GC will manage their lifetime
    auto obj = new MyClass();

    // prints 'variable = 0', ints are initialized to 0 by default
    writeln("variable = ", obj.variable);

    // invoke the method
    obj.someMethod();

    // prints 'variable = 1'
    writeln("variable = ", obj.variable);

    // set the variable using setter method
    obj.variable = 99;

    // prints 'variable = 99'
    writeln("variable = ", obj.variable);
}
