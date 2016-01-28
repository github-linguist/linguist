

terra bar(a : int)
	return 4,5
end
terra takebar(a : double, b : int)
	return 1
end
terra baz()
end
function whatwhat()
end
struct A { data : int }
terra A:moo() end
Aptr = terralib.new(A)
terra foo()
	var aa : A

	baz()
	whatwhat()
	do
	end
	::what::
	goto what
	while 4 < 3 do
	    break
	    terralib.attrload(&aa,{})
	    var d = terralib.attrstore(&aa,aa,{})
		return 3,4,4,bar(aa.data)
	end
	var a = 0.0
	if a < 3 then
		a = -(a + 1)
	end

	if a < 3 then
		a = -a + 1
	elseif a > 4 then
		a = a - 1
	end
	repeat
		a = a + 1
	until a > 55
	var b,c = 4,5
	a,b = 5,c
	var d = array(1,2,3)
	b = (&a)[1]
	var e : int = a
	var g = terralib.select(true,0,1)
	var ee = sizeof(int)
	var more = { a = 5, c = 4, _2 = 3}
	baz()
	var bbb = bar(1)
	var bb = takebar(bbb._0,bbb._1)
	var vv = vector(3,4)
	var vvv = Aptr
	var vvvv = Aptr:moo()
	return 3,4,ee,bar(1)
end

foo:compile()
foo:printpretty(false) --before compilation
foo:printpretty() --after compilation

local a = `1 + 2
local b = quote var c = a + a in a + 1 end

a:printpretty()
b:printpretty()