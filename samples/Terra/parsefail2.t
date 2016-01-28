terra foo() 
    var a : int[4] @&a[0] = 3 
    return a[0]
end
assert(3 == foo())