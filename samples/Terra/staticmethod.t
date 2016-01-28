struct A {
}
A.metamethods.__getmethod = function(self,idx)
    return tonumber(string.sub(idx,2,-1))
end
terra foo()
    return A.f33 + A.b34
end
assert(67 == foo())