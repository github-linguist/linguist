
terra foo(a : double)
    var v0 : vector(double,3) = a
    var v1 : vector(int,3) = 4
    var v2 = v0 / v1
    var v3 = (v0 <= v1) or (v1 >= v0)
    var ptr = [&double](&v2)
    return v3[0] or v3[1] or v3[2]
    --return ptr[0] + ptr[1] + ptr[2]
end


N = 64
terra addsomevecs(a : vector(double,N), b : vector(double,N), c : vector(double,N) )
	return (a + b) / c
end


terra foo2(a : double, b : double)
	var c = addsomevecs(a,b,b)
	return c[0] + c[1]
end

local test = require("test")

test.eq(foo(3), true)
test.eq(foo2(3,4), 3.5)