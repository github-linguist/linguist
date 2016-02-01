

struct A {
	a : int
}


terra what()
	var a : &A = nil
	return a + 4
end

what:compile()