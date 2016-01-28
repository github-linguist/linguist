function failit(fn)
	local success,msg = pcall(fn)
	if success then
		error("failed to fail.",2)
	elseif not string.match(msg,"Errors reported during") then
		error("failed wrong: "..msg,2)
	end
end

terra foo()
	return 1
end

a = 4
terra foo2()
	return a
end
b = 4.5
terra foo3()
	return b
end

terra foo4()
	var a = 3
	return a
end



sa = symbol("myname")

terra foo5()
	var [sa] = 5
	return sa
end

whatwhat = `3 + b

terra foo6()
	return whatwhat
end

local whatwhat2 = {4,5}

terra foo7()
	return 3,4,5,whatwhat,whatwhat2
end

terra foo8()
	return whatwhat2,4
end

local test = require("test")
assert(foo() == 1)
assert(foo2() == 4)
assert(foo3() == 4.5)
assert(foo4() == 3)
assert(foo5() == 5)
assert(foo6() == 7.5)
test.meq({3,4,5,7.5,4,5},foo7())
test.meq({4,5,4},foo8())

local a = { b = {c = 4}}
terra foo9()
	return a.["b"].c
end
print(foo9())

struct A { a : int }

A.methods.what = terra() return 4 end


terra foo10()
	return A.what()
end

assert(foo10() == 4)


terra foo11()
	var c = 3
	return [4 + 5 + a.b.c] + [c]
end

assert(foo11() == 16)

failit(function()
	local terra missingvar()
		return missing
	end
end)
local b = coroutine.create(failit)
failit(function()
	return `sa.foobar,`b.e
end)
failit(function()
	return `a.d
end)
failit(function()
	return `a.[1+"nope"]
end)
failit(function()
	return `a.[1]
end)

over = symbol()

anint = symbol(int)

terra foo13()
	var [anint]
	anint = 3
	return anint
end
assert(foo13() == 3)

terra foo12()
	goto [over]
	while true do
	end
	::[over]::
end

foo12()


terra foo14() : {int,double}
	return 3.5,4.5
end

test.meq({3,4.5},foo14())

terra foo15() : int
	return 3.5
end
assert(foo15() == 3)

failit(function() 
	local terra foo16() : 3
	end
end)

terra foo16()
	var r = 0
	for i = 0,10 do
		r = r + 2
	end
	return r
end

assert(foo16() == 20)

terra foo17()
	var i = 0
	repeat 
		i = i + 1
		var b = i + 1
	until b == 10
	repeat
	until true
	return i
end
assert(foo17() == 9)

local terra foo18()
	var a : int, b : double = 2.5,2.5
	return a,b
end

test.meq({2,2.5},foo18())

failit(function()
	local function doit()
		return 1 + {}
	end
	return `[doit()]
end)

failit(function()
local terra foo19()
	var a : 3, b : double = 2.5,2.5
	return a,b
end
foo19()
end)

failit(function()
local terra foo20()
	var a : int, a : double = 2.5,2.5
	return a,a
end
foo20()
end)

foo21s = symbol()

local terra foo21()
	var [foo21s],b = 3,4
	return b + foo21s
end
assert(foo21() == 7)

failit(function()
local terra foo20()
	var ["a"] : int, ["b"] = 4,5
	return a
end
end)

failit(function()
	return quote var a end
end)

astring = "astring"

local terra foo22()
	return astring[0]
end

assert(foo22() == 97)

local aquote = `7
local atree = aquote.tree

terra foo23()
	return atree
end

assert(foo23() == 7)

terra foo24(a : int)
	return 3,4 + a
end

terra foo25()
	var c,a,b = 4,foo24(3)
	return a + b + c
end



assert(foo25() == 14)

local obfuscate = { foo24 }
terra foo26()
	var c,a,b = 4,obfuscate(3)
	return a + b + c
end

local obfuscate2 = { `foo24(3) }
terra foo27()
	obfuscate2
	foo24(3)
end
foo27()

assert(foo26() == 14)

failit(function()
local terra foo26a(a : int)
	if a == 0 then
		return a
	else
		return foo26a(a - 1) + 1
	end
end
foo26a(3)
end)

local terra foo26b(a : int) : int
	if a == 0 then
		return a
	else
		var foo26bptr = foo26b
		return foo26b(a - 1) + foo26bptr(0) + 1
	end
end

assert(foo26b(3) == 3)


failit(function()
	(terra() return (3)(4) end)()
end)

failit(function()
	local terra notdefined
	local terra callnotdefined()
		return notdefined(1)
	end
	callnotdefined()
end)


	local terra norets()
	end
	local terra callnotdefined()
		return (norets())
	end
	callnotdefined()

local terra returns2() return 1,2 end

terra foo29(a : int)
	if a > 1 then
		return (returns2()._0)
	else
		return 5
	end
end

assert(foo29(3) == 1 and foo29(0) == 5)


terra foo30()
	print(4)
end

foo30()


local terra notdefined
local terra definedtwice(a : int) return a end
terra definedtwice(a : int, b : int) return a + b end

failit(function()
local terra foo31()
	return (notdefined), (definedtwice)
end
foo31()
end)

terra foo35()
	return definedtwice(3) + definedtwice(3,4)
end


local terra foo36()
    var r = returns2()
	return definedtwice(unpackstruct(r))
end
assert(3 == foo36())

assert(foo35() == 10)

struct B {
	a : int;
	b : double;
}

terra B:add()
	return self.a + self.b
end

terra B:inc(v : int)
	self.a = self.a + v
end

B.metamethods.__apply = terra(self : &B, v :int)
	return v + v
end

B.metamethods.__entrymissing = macro(function() return 8 end)

struct C {
	a : int;
}

terra B.metamethods.__add(self : &B, a : int, b : int)
	return self.a + a
end
terra B.metamethods.__add(self : &B, a : int, b : int, c : int)
	return self.a + a
end


function myvoid()
	print("CALLED")
end
terra testcallvoid()
	myvoid()
end
print("BEFORE")
testcallvoid()
print("AFTER")

terra C.metamethods.__add(b : &B, c : &C)
	return b.a + c.a
end

terra foo40()
	var b = B { 5,6 }
	var c = C { 3 }
	var ptrb =  &b
	b:inc(3)
	ptrb:inc(3)
	return b:add() + ptrb:add() + b(3) + ptrb(3) + b.foo + ptrb.foo + (b + c)
end

assert(foo40() == 8 + 8 + 6 + 6 + 6 + 11 + 6 + 11 + 3 + 11)
B.metamethods.__add = macro(function(b,c) return `b.a + c.a + 1 end)
terra foo41()
	var b = B { 5,6 }
	var c = C { 3 }
	var ptrb =  &b
	b:inc(3)
	ptrb:inc(3)
	return b:add() + ptrb:add() + b(3) + ptrb(3) + b.foo + ptrb.foo + (b + c)
end

assert(foo41() == 8 + 8 + 6 + 6 + 6 + 11 + 6 + 11 + 3 + 11 + 1)

terra foo32()
	var a = B { b = 3, a = 4 }
	var ptra = &a
	a.b = a.b + 1
	ptra.a = ptra.a + (@ptra).a + 1
	return a.a, a.b
end

test.meq({9,4},foo32())

terra foo33()
	return (vector(3,4,5) + vectorof(double,3,4,5))[0] + sizeof(double)
end

local notinscope = symbol()

failit(function()
	local terra foo34()
		return notinscope
	end
	foo34()
end)

local gbl = global(int)

terra foo37()
	gbl = 4
end
foo37()

assert(gbl:get() == 4)
assert(foo33() == 14)

local C = terralib.includec("stdio.h")

terra foo38()
	C.printf("hello, world %f\n", 3.5f)
end
foo38()
local twice = macro(function(exp,call)
	return quote
		exp = exp + 1 + call._0
		exp = exp + 1 + call._0
	end
end)
terra foo39()
	var a = 4
	twice(a,returns2())
	return a
end

assert(foo39() == 8)
--checkexp truncate and allowluaobjects behavior
--global variable resolution
--select/pointerselect/__get
--expression macro 1 return
--expression macro truncated
--expression macro expanded
--expression macro that returns a multi-ret function
--expression macro that returns a multi-ret function that was an argument
--function call, function call with multi-return, 
--function call to overloaded function/undef function
--overloaded operator with mixed macro/function stuff
--reciever cast stuff