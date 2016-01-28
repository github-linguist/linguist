C = terralib.includecstring [[
    enum  Foo {
        A_VALUE = -1,
        B_VALUE = 4,
        C_VALUE = 2 << 1,
        D_VALUE = (1 << 31) - 1,
        E_VALUE = (1 << 44)
    };
]]
assert(C.A_VALUE == -1)
assert(C.B_VALUE == 4)
assert(C.C_VALUE == 4)
assert(C.D_VALUE == 2147483647)
assert(C.E_VALUE == -2147483648)