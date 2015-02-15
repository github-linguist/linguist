import std.stdio;

struct Catcher {
    void foo() { writeln("This is foo"); }

    void bar() { writeln("This is bar"); }

    void opDispatch(string name, ArgsTypes...)(ArgsTypes args) {
        writef("Tried to handle unknown method '%s'", name);
        if (ArgsTypes.length) {
            write(", with arguments: ");
            foreach (arg; args)
                write(arg, " ");
        }
        writeln();
    }
}

void main() {
    Catcher ca;
    ca.foo();
    ca.bar();
    ca.grill();
    ca.ding("dong", 11);
}
