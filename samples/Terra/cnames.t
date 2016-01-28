struct Foo {
    c : float;
}
Foo.displayname = "struct.Foo"
terra useFoo()
    var a : Foo
    a.c = 4.5
    return a.c
end
assert(4.5 == useFoo())

C = terralib.includecstring [[
typedef struct { int a; int b; } Foo;
]]

terra stuff()
    var a : Foo
    var b : C.Foo
    b.a = 1
    b.b = 2
    a.c = 4.5
    return b.a + b.b + a.c
end

assert(7.5 == stuff())
