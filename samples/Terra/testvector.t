
local S = require "std"

local floatv = S.Vector(int)
terra foo()
    var a = floatv.salloc():init()
    for i = 0,100 do
        a:insert(i)
    end
    for i = 0,50 do
        a:insert(0,i)
    end
    for i = 0,50 do
        a:insert(1,2,i)
    end
    for i = 0ULL,a:size() do
       a(i) = a(i) + 1
    end
    for i = 0ULL,50 do
        @a:insert() = 5
    end
    var s = 0
    for i = 0ULL,a:size() do
        s = s + a(i)
    end
    var z = a:size()
    S.assert(a:remove() == 5)
    S.assert(a:size() == z - 1)
    S.assert(a:remove(0) == 50)
    S.assert(a:remove(0) == 50)
    S.assert(a:remove(0) == 50)
    S.assert(a(1) == 49)
    return s
end
assert(foo() == 8875 + 5 *50)

local g = global(int,0)

struct A (S.Object) {
    a : int
}

terra A:__destruct() 
    g = g + self.a
end

assert(S.Vector(int) == S.Vector(int))

terra foo2()
    var a = [S.Vector(A)].salloc():init()
    for i = 0,50 do
        a:insert().a = i
    end
end

foo2()
assert(g:get() == 49*50/2)