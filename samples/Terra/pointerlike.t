
terra foo()
    var a : int[4]
    var b : &int = a
    return a + 1, a - a, b - a, a - b, 1 + a 
end
foo()