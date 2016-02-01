
struct B {
   a : A
} and
struct A {
   b : &B
}




struct C {
    i : int
}
local U = struct { c : C }
local UP = &U


local FP = UP -> int
local FP2 = UP -> int

local FI = int -> int
local FI2 = int -> int
terra anon()
    var b : B
    b.a.b = &b
    return 4
end

terra anon2()
    var u = U { C {3} }
    
    var fp : FP, fi : FI
    var fi2 : FI2 = fi
    var fp2 : FP2 = fp
    var up : UP = &u
    return up.c.i
end



test = require("test")

test.eq(anon(),4)
test.eq(anon2(),3)