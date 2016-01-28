

struct A {
}

A.metamethods.__luametatable = { __index = function(self,idx) return 4 end }
A.methods.foo = function() return 5 end
a = terralib.new(A)

assert(a.foo == 4)