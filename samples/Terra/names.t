struct A {
}

local B = (function() 
    local struct A {}
    return A
end)()
C = terralib.types.newstruct("A$1")
D = terralib.types.newstruct("A$1")

local names = {}
for i,t in ipairs {A,B,C,D} do
    local n = tostring(t)
    assert(not names[n])
    names[n] = true
end