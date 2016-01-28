C = terralib.includecstring [[
#define foo 1
#define foo2 -3.4
#undef foo
#define foo 3
]]

assert(C.foo == 3)
assert(C.foo2 == -3.4)