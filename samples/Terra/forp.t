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
    var v = Range { 0, 3 } 
    var vp = &v
    var i = 0
    for e in vp do i = i + e end
    return i
end
assert(3 == foo())