G,T = terralib.includecstring [[
    typedef struct {
        double c;
    } A;
    struct A {
        int b;
    };
]]

G2,T2 = terralib.includecstring [[
    struct A;
    typedef struct C A;
]]

assert(T2.A == T.A)
assert(G2.A ~= T2.A and G2.A ~= G.A)
terra foo()
    var a : G.A
    var b : T.A
    a.c = 4.5
    b.b = 4.5
    return a.c + b.b
end
assert(8.5 == foo())