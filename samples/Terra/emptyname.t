struct A {}
A.metamethods.__typename = function() return "" end
struct B {}
B.metamethods.__typename = function() return "" end
print(A:cstring(),B:cstring())