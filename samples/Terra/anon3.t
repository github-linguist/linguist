C,T = terralib.includecstring [[
    struct D {
        struct { int a; struct { int c; } c; } b;
    } c;
    typedef struct {int c;} B;
    typedef struct {
        struct { int a; } a;
        struct { int b; } b;
        B c;
    } A;
]]

terra foo(a : &C.A)
   var c = a.a
   var b = a.b.b
   return c.a + b + a.c.c;
end
foo:compile()