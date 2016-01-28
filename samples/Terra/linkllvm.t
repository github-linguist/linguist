

terra add(a : int, b : int)
    return a + b
end


terralib.saveobj("add.bc",{ add = add })

local addlib = terralib.linkllvm("add.bc")
add2 = addlib:extern("add", {int,int} -> int)

assert(add2(3,4) == 7)