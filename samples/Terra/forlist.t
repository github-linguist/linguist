

struct Range {
    a : int;
    b : int;
}
Range.metamethods.__for = function(syms,iter,body)
    return syms, quote
        var it = iter
        for [syms[1]] = it.a,it.b do
            body
        end
    end
end

terra foo()
    var a = 0
    for i in Range {0,10} do
        a = a + i
    end
    return a
end

assert(foo() == 10*9/2)