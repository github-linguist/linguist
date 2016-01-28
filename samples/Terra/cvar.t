C = terralib.includecstring [[
    int foo = 4;
    const int foo2 = 5;
]]
terra t()
    C.foo = C.foo + 1;
    return C.foo + C.foo2
end
assert(t() == 10)