function dog(...)
	print("Hi. This is Dog.",...)
end

terra foo()
	dog()
end

terra passanarg()
	dog(3,4,5,nil)
end

function takesastruct(a)
	print(a.a,a.b)
	a.a = a.a + 1
end
struct A { a : int , b : double }
terra passastruct()
	var a = A {1,3.4}
	--takesastruct(a) luajit doesn't like having structs passed to its callbacks by value?
	var b = a.a
	takesastruct(&a)
	var c = a.a
	return b + c
end

foo()
passanarg()

local test = require("test")

test.eq(passastruct(),3)
