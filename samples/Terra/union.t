
struct A { a : int, union { c : int8[6], b : int }, d : float, union { e : int, f : float }, g : int }
struct B { union { b : int , c : int8[6] }, a : int }
local C = struct {  union { union { b : int}  , c : int8[6] }, w : int }
terra foo()
	var a : A
	var b : B
	var c : C
	a.b = 4
	a.a = 5
	a.c[4] = 3
	a.d = 4
	return a.a + a.b + a.c[4] + a.d
end

struct Stuff { union { a : int, b : float } }

terra what()
	var a : Stuff, b : Stuff
	a.a,b.a = 3,4
	return a.b/b.b --== .75, this is the best bad interview question
end

local test = require("test")
test.eq(foo(),16)
test.eq(what(),.75)