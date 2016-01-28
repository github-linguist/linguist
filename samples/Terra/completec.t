C = terralib.includecstring [[
    struct A {
        int a;
    };
    static struct A foo;
    struct A * getfoo() {
        foo.a = 4;
        return &foo;
    }
]]

assert(C.getfoo().a == 4)

