/// Declare a function with no arguments that returns an integer.
int noArgs();

/// Declare a function with no arguments that returns an integer.
int twoArgs(int a, int b);

/// Parameter names are optional in a prototype definition.
int twoArgs2(int, int);

/// An ellipsis can be used to declare a function that accepts
/// C-style varargs.
int anyArgs(...);

/// One mandatory integer argument followed by C-style varargs.
int atLeastOneArg(int, ...);

/// Declare a function that accepts any number of integers.
void anyInts(int[] a...);

/// Declare a function that accepts D-style varargs.
void anyArgs2(TArgs...)(TArgs args);

/// Declare a function that accepts two or more D-style varargs.
void anyArgs3(TArgs...)(TArgs args) if (TArgs.length > 2);

/// Currently D doesn't support named arguments.

/// One implementation.
int twoArgs(int a, int b) {
    return a + b;
}

interface SomeInterface {
    void foo();
    void foo(int, int);

    // Varargs
    void foo(...); // C-style.
    void foo(int[] a...);
    void bar(T...)(T args); // D-style.

    // Optional arguments are only supported if a default is provided,
    // the default arg(s) has/have to be at the end of the args list.
    void foo(int a, int b = 10);
}

void main() {}
