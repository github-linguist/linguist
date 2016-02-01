terra foo(a : bool, b : bool, c : bool) return a and b or c end

foo:disas()
terra callfoo()
    var r = foo(true,false,true)
    return r
end
assert(callfoo())