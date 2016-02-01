
terra foo() : {double, double}
	return 1.0,3.0
end

struct A {c : int, a : int, b : double }

terra bar()
    var r = foo()
	var a  = A {1,unpackstruct(r)}
	var b  = A {1,2,(foo())._0}
	var c  = A {c = 1,a = 2,b = foo()._0}
	return a.c + a.a + a.b + b.c + c.c
end

local test = require("test")
test.eq(7,bar())