

struct A {
	a : int
}

struct B {
	a : A
}

function A.metamethods.__staticinitialize()
	print("STATIC INIT A")
	local terra what(b : B)
	end
	what:gettype(true)
end

function B.metamethods.__staticinitialize()
	print("STATIC INIT B")
end

terra foo(b : B)
end

foo:compile()