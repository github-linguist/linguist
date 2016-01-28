struct A {
	a : int;
	b : int;
	c : &B
} and struct B {
	a : &A
}

local a = global(A)


terra foo()
	var b : B
	return a.a + a.b
end

function A.metamethods.__staticinitialize(self)
	print("A")
	assert(A:iscomplete())
	a:get().a = 4
end

function B.metamethods.__staticinitialize(self)
	print("B")
	assert(B:iscomplete())
	a:get().b = 3
	foo:gettype(function()
		assert(foo() == 7)
		a:get().a = a:get().a + 1
	end)
end

assert(foo() == 8)