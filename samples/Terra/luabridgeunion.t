struct A { union {a : int, b : double}, c : int }

terra foo(a : A)
	return a.a + a.c
end
terra foo2(a : A)
	return a.b + a.c
end


local test = require("test")

test.eq( foo({a = 4, c = 6}), 10)
test.eq( foo2({b = 4.4, c = 6}), 10.4)